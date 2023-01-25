using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.FindSymbols;
using Microsoft.CodeAnalysis;
using System;
using System.Linq;

namespace SyntaxWriter.Writers
{
    public class UnusedMethodRemover : CSharpSyntaxRewriter
    {
        private readonly Compilation Compilation;
        private readonly Solution Solution;
        private readonly SemanticModel Semantics;

        public UnusedMethodRemover(Compilation compilation, Solution solution, SyntaxTree syntaxTree)
        {
            Compilation = compilation;
            Solution = solution;
            Semantics = Compilation.GetSemanticModel(syntaxTree);
        }

        public override SyntaxNode Visit(SyntaxNode node)
        {
            if (node == null)
            {
                return null;
            }

            if (node is MethodDeclarationSyntax methodDeclration
                && IsUnusedMethod(methodDeclration))
            {
                Say(methodDeclration.Identifier.Text, ConsoleColor.Red);
                return null;
            }

            return base.Visit(node);
        }

        private bool IsUnusedMethod(MethodDeclarationSyntax methodDeclration)
        {
            var symbol = Semantics.GetDeclaredSymbol(methodDeclration);

            Say($"{methodDeclration.Identifier.Text}");

            var hasAnyReferences = SymbolFinder.FindReferencesAsync(symbol, Solution)
                .Result
                .Any();
            return !hasAnyReferences;
        }

        private void Say(object o, ConsoleColor backgroundColor = ConsoleColor.Gray, ConsoleColor textColor = ConsoleColor.Black)
        {
            Console.BackgroundColor = backgroundColor;
            Console.ForegroundColor = textColor;
            Console.WriteLine(o);
            Console.ResetColor();
        }
    }
}
