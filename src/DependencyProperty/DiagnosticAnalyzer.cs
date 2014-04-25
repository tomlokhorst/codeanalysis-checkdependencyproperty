using System;
using System.Collections.Immutable;
using System.Threading;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace DependencyPropertyNullable
{
  [DiagnosticAnalyzer]
  [ExportDiagnosticAnalyzer(DiagnosticId, LanguageNames.CSharp)]
  public class DiagnosticAnalyzer : ISyntaxNodeAnalyzer<SyntaxKind>
  {
    internal const string DiagnosticId = "DependencyProperty";
    internal const string Category = "Conventions";

    internal const string OwnerTypeDescription = "Owner type does not match";
    internal const string OwnerTypeMessageFormat = "Specified ownerType '{0}' doesn't match enclosing type '{1}'";
    internal static DiagnosticDescriptor OwnerTypeRule = new DiagnosticDescriptor(DiagnosticId, OwnerTypeDescription, OwnerTypeMessageFormat, Category, DiagnosticSeverity.Warning);

    internal const string NameExistsDescription = "Property name doesn't exist";
    internal const string NameExistsMessageFormat = "There is no property named '{0}' in this class";
    internal static DiagnosticDescriptor NameExistsRule = new DiagnosticDescriptor(DiagnosticId, NameExistsDescription, NameExistsMessageFormat, Category, DiagnosticSeverity.Warning);

    internal const string NameDuplicateDescription = "DependencyProperty name is duplicate";
    internal const string NameDuplicateMessageFormat = "There is another DependencyProperty also named '{0}'";
    internal static DiagnosticDescriptor NameDuplicateRule = new DiagnosticDescriptor(DiagnosticId, NameDuplicateDescription, NameDuplicateMessageFormat, Category, DiagnosticSeverity.Warning);

    public ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics
    {
      get { return ImmutableArray.Create(OwnerTypeRule, NameExistsRule, NameDuplicateRule); }
    }

    public ImmutableArray<SyntaxKind> SyntaxKindsOfInterest
    {
      get { return ImmutableArray.Create(SyntaxKind.FieldDeclaration); }
    }

    private ArgumentListSyntax getArgumentList(FieldDeclarationSyntax field, SemanticModel semanticModel)
    {
      var typeName = semanticModel.GetTypeInfo(field.Declaration.Type).Type.ToDisplayString();
      if (typeName != "Windows.UI.Xaml.DependencyProperty") return null;

      var decl = field.Declaration.Variables.FirstOrDefault();
      if (decl == null) return null;

      var invoke = decl.Initializer.Value as InvocationExpressionSyntax;
      if (invoke == null) return null;

      var mae = invoke.Expression as MemberAccessExpressionSyntax;
      if (mae == null) return null;

      var typeSymbol = semanticModel.GetTypeInfo(mae.Expression).Type;
      if (typeSymbol == null || typeSymbol.ToDisplayString() != "Windows.UI.Xaml.DependencyProperty") return null;

      if (mae.Name.Identifier.ValueText != "Register") return null;

      if (invoke.ArgumentList.Arguments.Count < 4) return null;

      return invoke.ArgumentList;
    }

    public void AnalyzeNode(SyntaxNode node, SemanticModel semanticModel, Action<Diagnostic> addDiagnostic, CancellationToken cancellationToken)
    {
      var field = node as FieldDeclarationSyntax;
      if (field == null) return;

      var classDecl = field.Parent as ClassDeclarationSyntax;
      if (classDecl == null) return;

      var argumentList = getArgumentList(field, semanticModel);

      checkNameExists(classDecl, argumentList, addDiagnostic, semanticModel);
      checkNameDuplicate(classDecl, field, argumentList, addDiagnostic, semanticModel);
      checkOwnerType(classDecl, argumentList, addDiagnostic, semanticModel);
    }

    private void checkNameExists(ClassDeclarationSyntax classDecl, ArgumentListSyntax registerArgumentList, Action<Diagnostic> addDiagnostic, SemanticModel semanticModel)
    {
      var stringLiteral = registerArgumentList.Arguments[0].Expression as LiteralExpressionSyntax;
      if (stringLiteral == null || !stringLiteral.IsKind(SyntaxKind.StringLiteralExpression)) return;

      var name = stringLiteral.Token.ValueText;

      var properties = classDecl.Members.OfType<PropertyDeclarationSyntax>();
      var propertyNameExists = properties.Any(p => p.Identifier.ValueText == name);

      if (propertyNameExists) return;

      var diagnostic = Diagnostic.Create(NameExistsRule, stringLiteral.GetLocation(), name);
      addDiagnostic(diagnostic);
    }

    private void checkNameDuplicate(ClassDeclarationSyntax classDecl, FieldDeclarationSyntax field, ArgumentListSyntax registerArgumentList, Action<Diagnostic> addDiagnostic, SemanticModel semanticModel)
    {
      var stringLiteral = registerArgumentList.Arguments[0].Expression as LiteralExpressionSyntax;
      if (stringLiteral == null || !stringLiteral.IsKind(SyntaxKind.StringLiteralExpression)) return;

      var name = stringLiteral.Token.ValueText;

      var fields = classDecl.Members.OfType<FieldDeclarationSyntax>();
      var nameLiterals = fields
        .Where(f => f != field)
        .Select(f => getArgumentList(f, semanticModel))
        .Where(al => al != null)
        .Select(al => al.Arguments[0].Expression as LiteralExpressionSyntax)
        .Where(l => l != null);

      var nameDuplicatesExist = nameLiterals.Any(l => l.Token.ValueText == name);
      if (!nameDuplicatesExist) return;

      // If this field follows the *Property convention, ignore this one, the other duplicate wil show the message
      var followsConvention = field.Declaration.Variables.Any(v => v.Identifier.ValueText == name + "Property");
      if (followsConvention) return;

      var diagnostic = Diagnostic.Create(NameDuplicateRule, stringLiteral.GetLocation(), name);
      addDiagnostic(diagnostic);
    }

    private void checkOwnerType(ClassDeclarationSyntax classDecl, ArgumentListSyntax registerArgumentList, Action<Diagnostic> addDiagnostic, SemanticModel semanticModel)
    {
      var typeOf = registerArgumentList.Arguments[2].Expression as TypeOfExpressionSyntax;
      if (typeOf == null) return;

      var correctOwnerType = semanticModel.GetTypeInfo(typeOf.Type).Type == semanticModel.GetDeclaredSymbol(classDecl);

      if (correctOwnerType) return;

      var diagnostic = Diagnostic.Create(OwnerTypeRule, typeOf.GetLocation(), typeOf.Type, classDecl.Identifier.ValueText);
      addDiagnostic(diagnostic);
    }
  }
}
