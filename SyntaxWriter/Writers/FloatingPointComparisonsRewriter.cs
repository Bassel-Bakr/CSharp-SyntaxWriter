using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace SyntaxWriter.Writers
{
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
            { SyntaxKind.EqualsExpression, "ApproxEquals" },
            { SyntaxKind.NotEqualsExpression, "ApproxNotEquals" } ,
            { SyntaxKind.GreaterThanExpression, "ApproxGreaterThan" } ,
            { SyntaxKind.LessThanExpression, "ApproxLessThan" } ,
            { SyntaxKind.GreaterThanOrEqualExpression, "ApproxGreaterThanOrEquals" } ,
            { SyntaxKind.LessThanOrEqualExpression, "ApproxLessThanOrEquals" } ,
        };

        private Dictionary<SyntaxKind, string> SWAPPED_CONDITIONALS = new Dictionary<SyntaxKind, string>
        {
            { SyntaxKind.EqualsExpression, "ApproxEquals" },
            { SyntaxKind.NotEqualsExpression, "ApproxNotEquals" },
            { SyntaxKind.GreaterThanExpression, "ApproxLessThan" },
            { SyntaxKind.LessThanExpression, "ApproxGreaterThan" },
            { SyntaxKind.GreaterThanOrEqualExpression, "ApproxLessThanOrEquals" },
            { SyntaxKind.LessThanOrEqualExpression, "ApproxGreaterThanOrEquals" },
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
}
