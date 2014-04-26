using System;
using System.Collections.Immutable;
using System.Threading;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace CheckDependencyProperty
{
  [DiagnosticAnalyzer]
  [ExportDiagnosticAnalyzer(DiagnosticId, LanguageNames.CSharp)]
  public class PropertyDiagnosticAnalyzer : ISyntaxNodeAnalyzer<SyntaxKind>
  {
    internal const string DiagnosticId = "PropertyDependencyProperty";
    internal const string Category = "Conventions";

    internal const string DiagnosticDescription = "DependencyProperty refers to different property";
    internal const string DiagnosticMessageFormat = "DependencyProperty refers to '{0}' instead of this property ('{1}')";
    internal static DiagnosticDescriptor Rule = new DiagnosticDescriptor(DiagnosticId, DiagnosticDescription, DiagnosticMessageFormat, Category, DiagnosticSeverity.Warning);

    public ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics
    {
      get { return ImmutableArray.Create(Rule); }
    }

    public ImmutableArray<SyntaxKind> SyntaxKindsOfInterest
    {
      get { return ImmutableArray.Create(SyntaxKind.PropertyDeclaration); }
    }

    public void AnalyzeNode(SyntaxNode node, SemanticModel semanticModel, Action<Diagnostic> addDiagnostic, CancellationToken cancellationToken)
    {
      var property = node as PropertyDeclarationSyntax;
      if (property == null) return;

      var classDecl = property.Parent as ClassDeclarationSyntax;
      if (classDecl == null) return;

      var accessors = property.AccessorList.Accessors;
      var getter = accessors.FirstOrDefault(a => a.IsKind(SyntaxKind.GetAccessorDeclaration));
      var setter = accessors.FirstOrDefault(a => a.IsKind(SyntaxKind.SetAccessorDeclaration));

      if (getter != null)
      {
        var diagnostic = checkGetter(classDecl, property.Identifier.ValueText, getter, semanticModel);
        if (diagnostic != null)
          addDiagnostic(diagnostic);
      }

      if (setter != null)
      {
      //  var diagnostic = checkSetter(classDecl, setter, semanticModel);
      //  if (diagnostic != null)
      //    addDiagnostic(diagnostic);
      }

      var fields =
        from f in classDecl.Members.OfType<FieldDeclarationSyntax>()
        let args = FieldDiagnosticAnalyzer.GetArgumentList(f, semanticModel)
        where args != null
        let lit = args.Arguments[0].Expression as LiteralExpressionSyntax
        where lit.Token.ValueText == property.Identifier.ValueText
        select f;
      var field = fields.FirstOrDefault();
      if (field == null) return;
    }

    private FieldDeclarationSyntax getDpField(ClassDeclarationSyntax classDecl, string fieldName)
    {
      var fields =
        from f in classDecl.Members.OfType<FieldDeclarationSyntax>()
        where f.Declaration.Variables.Count > 0
        let variable = f.Declaration.Variables[0]
        where variable.Identifier.ValueText == fieldName
        select f;

      return fields.FirstOrDefault();
    }

    private Diagnostic checkGetter(ClassDeclarationSyntax classDecl, string propertyName, AccessorDeclarationSyntax accessor, SemanticModel semanticModel)
    {
      var statements = accessor.Body.Statements;
      if (statements.Count != 1) return null;

      var returnStatement = statements[0] as ReturnStatementSyntax;
      if (returnStatement == null) return null;

      var cast = returnStatement.Expression as CastExpressionSyntax;
      if (cast == null) return null;

      var invocation = cast.Expression as InvocationExpressionSyntax;
      if (invocation == null) return null;

      var symbolInfo = semanticModel.GetSymbolInfo(invocation.Expression).Symbol;
      if (symbolInfo.ToDisplayString() != "Windows.UI.Xaml.DependencyObject.GetValue(Windows.UI.Xaml.DependencyProperty)") return null;

      var arguments = invocation.ArgumentList.Arguments;
      if (arguments.Count != 1) return null;

      var identifierArgument = arguments[0].Expression as IdentifierNameSyntax;
      if (identifierArgument == null) return null;

      var field = getDpField(classDecl, identifierArgument.Identifier.ValueText);
      if (field == null) return null;

      var registerArguments = FieldDiagnosticAnalyzer.GetArgumentList(field, semanticModel);
      if (registerArguments == null) return null;

      var nameLiteral = registerArguments.Arguments[0].Expression as LiteralExpressionSyntax;
      if (nameLiteral == null || !nameLiteral.IsKind(SyntaxKind.StringLiteralExpression)) return null;
      var nameValueText = nameLiteral.Token.ValueText;

      if (nameValueText == propertyName) return null;

      return Diagnostic.Create(Rule, identifierArgument.GetLocation(), nameValueText, propertyName);
    }
  }
}
