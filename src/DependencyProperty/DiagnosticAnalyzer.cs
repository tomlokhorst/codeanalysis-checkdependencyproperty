using System;
using System.Collections.Immutable;
using System.Threading;
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
    internal const string Description = "Owner type does not match";
    internal const string MessageFormat = "Specified ownerType '{0}' doesn't match enclosing type '{1}'";
    internal const string Category = "Conventions";

    internal static DiagnosticDescriptor Rule = new DiagnosticDescriptor(DiagnosticId, Description, MessageFormat, Category, DiagnosticSeverity.Warning);

    public ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics
    {
      get { return ImmutableArray.Create(Rule); }
    }

    public ImmutableArray<SyntaxKind> SyntaxKindsOfInterest
    {
      get { return ImmutableArray.Create(SyntaxKind.FieldDeclaration); }
    }

    public void AnalyzeNode(SyntaxNode node, SemanticModel semanticModel, Action<Diagnostic> addDiagnostic, CancellationToken cancellationToken)
    {
      var field = node as FieldDeclarationSyntax;
      if (field == null) return;

      var classDecl = field.Parent as ClassDeclarationSyntax;
      if (classDecl == null) return;

      var typeName = semanticModel.GetTypeInfo(field.Declaration.Type).Type.ToDisplayString();
      if (typeName != "Windows.UI.Xaml.DependencyProperty") return;

      var decl = field.Declaration.Variables.FirstOrDefault();
      if (decl == null) return;

      var invoke = decl.Initializer.Value as InvocationExpressionSyntax;
      if (invoke == null) return;

      var mae = invoke.Expression as MemberAccessExpressionSyntax;
      if (mae == null) return;

      var typeSymbol = semanticModel.GetTypeInfo(mae.Expression).Type;
      if (typeSymbol == null || typeSymbol.ToDisplayString() != "Windows.UI.Xaml.DependencyProperty") return;

      if (mae.Name.Identifier.ValueText != "Register") return;

      if (invoke.ArgumentList.Arguments.Count < 4) return;

      checkOwnerType(classDecl, addDiagnostic, invoke.ArgumentList, semanticModel);
    }

    private void checkOwnerType(ClassDeclarationSyntax classDecl, Action<Diagnostic> addDiagnostic, ArgumentListSyntax registerArgumentList, SemanticModel semanticModel)
    {
      var typeOf = registerArgumentList.Arguments[2].Expression as TypeOfExpressionSyntax;
      var correctOwnerType = semanticModel.GetTypeInfo(typeOf.Type).Type == semanticModel.GetDeclaredSymbol(classDecl);

      if (correctOwnerType) return;

      var diagnostic = Diagnostic.Create(Rule, typeOf.GetLocation(), typeOf.Type, classDecl.Identifier.ValueText);
      addDiagnostic(diagnostic);
    }
  }
}
