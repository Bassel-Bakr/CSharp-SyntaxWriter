﻿using System;
using System.IO;
using Microsoft.CodeAnalysis.MSBuild;
using Microsoft.CodeAnalysis.CSharp;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.Build.Locator;
using System.Collections.Generic;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Configuration;

namespace SyntaxWriter
{
    public class Program
    {
        private static string SOLUTION_PATH = ConfigurationManager.AppSettings["Solution.Path"];

        public static void Main(string[] args)
        {
            if (!MSBuildLocator.IsRegistered)
            {
                var instances = MSBuildLocator.QueryVisualStudioInstances().ToArray();
                MSBuildLocator.RegisterInstance(instances.OrderByDescending(x => x.Version).First());
            }

            var workspace = MSBuildWorkspace.Create();
            workspace.SkipUnrecognizedProjects = true;
            workspace.WorkspaceFailed += (sender, diagnosticArgs) =>
            {
                if (diagnosticArgs.Diagnostic.Kind == WorkspaceDiagnosticKind.Failure)
                {
                    Console.Error.WriteLine(diagnosticArgs.Diagnostic.Message);
                }
            };

            //            const string dummySrc =
            //@"
            //            using System;
            //            using System.Collections;
            //            using System.Linq;
            //            using System.Text;

            //            namespace HelloWorld
            //            {
            //                class Program
            //                {
            //                    static void Main(string[] args)
            //                    {
            //                        var x = 0;
            //                        var y = 0;
            //                        var z = 0.0;
            //                        var zz = (z += 3) == 5.6;
            //                        double? nz = null;

            //                        var a = x - y == z;
            //                        var b = x != y;
            //                        var c = x > y;
            //                        var d = x < y;
            //                        var e = x >= y;
            //                        var f = x <= y;
            //                        var g = x + y == z;
            //                        var h = (double) x == y + z;
            //                        var i = x + y + z == 0;
            //                        var j = 0.4 == x + y + z;
            //                        var k = x + y + z == x + y + z;
            //                        var l = --y == z++;
            //                        var m = x++ == --z;
            //                        var n = -x == --z;
            //                        var o = ++z == --z;
            //                        var p = Math.Abs(0.0 - 4.5) == --z;
            //                        var q = nz < 3;
            //                    }
            //                }
            //            }";

            //            var syntaxTree = CSharpSyntaxTree.ParseText(dummySrc);

            //            var root = (CompilationUnitSyntax)syntaxTree.GetRoot();
            //            var compilation = CSharpCompilation.Create("HelloWorld")
            //                .AddReferences(MetadataReference.CreateFromFile(typeof(object).Assembly.Location))
            //                .AddSyntaxTrees(syntaxTree);

            //            syntaxTree.GetRoot()
            //                .DescendantNodes()
            //                .OfType<BinaryExpressionSyntax>()
            //                .ToList()
            //                .ForEach(expression =>
            //                {
            //                    Console.WriteLine(expression);
            //                    var semanticModel = compilation.GetSemanticModel(expression.SyntaxTree);
            //                    Console.WriteLine(semanticModel.GetTypeInfo(expression.Left).Type.Name);
            //                });


            //var syntaxRewriter = new FloatingPointComparisonsRewriter(null);

            //var newRoot = syntaxRewriter.Visit(syntaxTree.GetRoot());
            //var newSyntaxTree = syntaxTree.WithRootAndOptions(newRoot, syntaxTree.Options);

            //Console.WriteLine(syntaxTree);
            //Console.WriteLine(newSyntaxTree);

            //newSyntaxTree.GetRoot().WriteTo(newSyntaxTree.FilePath);

            ParseSolution(workspace, surround: true);

            Console.WriteLine("Done");

            Console.ReadKey();
            Console.ReadKey();
        }

        private static void ParseSolution(MSBuildWorkspace workspace, bool surround = false)
        {
            var solution = workspace.OpenSolutionAsync(SOLUTION_PATH,
                new ProgressBarProjectLoadStatus()).Result;

            var compilations = solution.Projects
                .Select(project => project.GetCompilationAsync().Result)
                .Skip(2);

            compilations.ToList().ForEach(compilation =>
            {
                ParseProjectCompilation(compilation, surround: surround);
            });
        }

        private static void ParseProjectCompilation(Compilation compilation, bool surround = false)
        {
            var syntaxTrees = compilation.SyntaxTrees;

            var syntaxWriter = new FloatingPointComparisonsRewriter(compilation, surround: surround);

            var updatedSyntaxTrees = syntaxTrees.Select(syntaxTree =>
            {
                var newRoot = syntaxWriter.Visit(syntaxTree.GetRoot());
                var newSyntaxTree = syntaxTree.WithRootAndOptions(newRoot, syntaxTree.Options);
                return new { Old = syntaxTree, New = newSyntaxTree };
            });

            // Skip unmodified trees
            updatedSyntaxTrees
                .Where(args => !args.Old.IsEquivalentTo(args.New))
                .Select(args => args.New)
                .ToList()
                .ForEach(syntaxTree =>
                {
                    using (var fileWriter = new StreamWriter(syntaxTree.FilePath))
                    {
                        syntaxTree.GetRoot().WriteTo(fileWriter);
                    }
                });
        }
    }

    public class FloatingPointComparisonsRewriter : CSharpSyntaxRewriter
    {
        private readonly Compilation Compilation;
        private readonly bool Surround = false;

        public FloatingPointComparisonsRewriter(Compilation compilation, bool surround = false)
        {
            Compilation = compilation;
            Surround = surround;
        }

        private Dictionary<SyntaxKind, string> CONDITIONALS = new Dictionary<SyntaxKind, string>
        {
            { SyntaxKind.EqualsExpression, "Equals" },
            { SyntaxKind.NotEqualsExpression, "NotEquals" } ,
            { SyntaxKind.GreaterThanExpression, "GreaterThan" } ,
            { SyntaxKind.LessThanExpression, "LessThan" } ,
            { SyntaxKind.GreaterThanOrEqualExpression, "GreaterThanOrEqual" } ,
            { SyntaxKind.LessThanOrEqualExpression, "LessThanOrEqual" } ,
        };

        private Dictionary<SyntaxKind, string> SWAPPED_CONDITIONALS = new Dictionary<SyntaxKind, string>
        {
            { SyntaxKind.EqualsExpression, "Equals" },
            { SyntaxKind.NotEqualsExpression, "NotEquals" },
            { SyntaxKind.GreaterThanExpression, "LessThan" },
            { SyntaxKind.LessThanExpression, "GreaterThan" },
            { SyntaxKind.GreaterThanOrEqualExpression, "LessThanOrEqual" },
            { SyntaxKind.LessThanOrEqualExpression, "GreaterThanOrEqual" },
        };

        private static Type[] FLOATING_POINT_TYPES = { typeof(double), typeof(decimal), typeof(float) };

        private static Predicate<ITypeSymbol> IsFloatingPointType = (ITypeSymbol type) =>
        {
            return FLOATING_POINT_TYPES.Select(floatingType => floatingType.Name)
                .Contains(type?.Name);
        };

        private static Predicate<TypeInfo> IsNullableFloatingTypeOnly = (TypeInfo type) =>
        {
            if (type.Type?.Name == typeof(Nullable).Name && type.Type is INamedTypeSymbol namedType
                && !namedType.TypeArguments.IsEmpty
                && IsFloatingPointType(namedType.TypeArguments.First()))
            {
                return true;
            }

            return false;
        };

        private Predicate<TypeInfo> IsNullableFloatingPointType = (TypeInfo type) =>
        {
            var nonNullable = IsFloatingPointType(type.Type);

            if (nonNullable)
            {
                return true;
            }

            if (IsNullableFloatingTypeOnly(type))
            {
                return true;
            }

            return false;
        };

        public override SyntaxNode Visit(SyntaxNode node)
        {
            if (node == null)
            {
                return null;
            }

            if (IsFloatingPointComparison(node))
            {
                var comparison = node as BinaryExpressionSyntax;
                var semanticModel = Compilation.GetSemanticModel(node.SyntaxTree);
                var leftIsNotFloating = !IsNullableFloatingPointType(semanticModel.GetTypeInfo(comparison.Left));
                var methodIdentifier = leftIsNotFloating ? SWAPPED_CONDITIONALS[comparison.Kind()] : CONDITIONALS[comparison.Kind()];

                return ReplaceComparisonExpression(semanticModel, comparison, methodIdentifier, leftIsNotFloating);
            }

            return base.Visit(node);
        }

        private bool IsFloatingPointComparison(SyntaxNode node)
        {
            if (!(node is BinaryExpressionSyntax comparison
                && CONDITIONALS.Keys.Contains(comparison.Kind())))
            {
                return false;
            }

            var semanticModel = Compilation.GetSemanticModel(comparison.SyntaxTree);

            var expressionSymbol = semanticModel.GetTypeInfo(comparison);
            var leftSymbolInfo = semanticModel.GetTypeInfo(comparison.Left);
            var rightSymbolInfo = semanticModel.GetTypeInfo(comparison.Right);

            return IsNullableFloatingPointType(leftSymbolInfo) || IsNullableFloatingPointType(rightSymbolInfo);
        }

        private SyntaxNode ReplaceComparisonExpression(SemanticModel semanticModel, BinaryExpressionSyntax comparison, string memberIdentifier, bool swapOperands = false)
        {
            var leftOperand = !swapOperands ? comparison.Left : comparison.Right;
            var rightOperand = !swapOperands ? comparison.Right : comparison.Left;

            var isLeftNullable = IsNullableFloatingTypeOnly(semanticModel.GetTypeInfo(leftOperand));

            leftOperand = leftOperand.WithoutTrivia();
            rightOperand = rightOperand.WithoutTrivia();

            var leftMustBeWrappedInParentheses =
                leftOperand.IsKind(SyntaxKind.NumericLiteralExpression)
                || leftOperand.IsKind(SyntaxKind.PostIncrementExpression)
                || leftOperand.IsKind(SyntaxKind.PreIncrementExpression)
                || leftOperand.IsKind(SyntaxKind.PostDecrementExpression)
                || leftOperand.IsKind(SyntaxKind.PreDecrementExpression)
                || leftOperand.IsKind(SyntaxKind.UnaryPlusExpression)
                || leftOperand.IsKind(SyntaxKind.UnaryMinusExpression)
                || (leftOperand.ChildNodes().Count() > 1
                    && !leftOperand.IsKind(SyntaxKind.InvocationExpression)
                    && !leftOperand.IsKind(SyntaxKind.SimpleMemberAccessExpression));

            leftOperand = leftMustBeWrappedInParentheses ? SyntaxFactory.ParenthesizedExpression(leftOperand) : leftOperand;


            ExpressionSyntax newNode;

            if (isLeftNullable)
            {
                var conditionalAccessExpression = SyntaxFactory.ConditionalAccessExpression(
                    leftOperand,
                    SyntaxFactory.MemberBindingExpression(
                        SyntaxFactory.IdentifierName(memberIdentifier)
                    )
                );

                newNode = SyntaxFactory.InvocationExpression(conditionalAccessExpression)
                    .WithArgumentList(
                        SyntaxFactory.ArgumentList(
                            SyntaxFactory.SingletonSeparatedList(
                                SyntaxFactory.Argument(rightOperand)
                            )
                        )
                    );
            }
            else
            {
                var memberAccessExpression = SyntaxFactory.MemberAccessExpression(
                    SyntaxKind.SimpleMemberAccessExpression,
                    leftOperand,
                    SyntaxFactory.IdentifierName(memberIdentifier))
                .WithOperatorToken(SyntaxFactory.Token(SyntaxKind.DotToken));

                newNode = SyntaxFactory.InvocationExpression(memberAccessExpression,
                SyntaxFactory.ArgumentList(
                    SyntaxFactory.SingletonSeparatedList(SyntaxFactory.Argument(rightOperand)))
                );
            };

            if (Surround)
            {
                newNode = SyntaxFactory.ParenthesizedExpression(newNode);
            }

            return newNode.WithTriviaFrom(comparison);
        }
    }

    public class ProgressBarProjectLoadStatus : IProgress<ProjectLoadProgress>
    {
        public void Report(ProjectLoadProgress progress)
        {
            Console.WriteLine($"{progress.Operation} {progress.FilePath} {progress.ElapsedTime} {progress.TargetFramework}");
        }
    }
}