using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace Soloution
{
    internal static class StaticClass
    {
        public class ExpressionExtensionsException : Exception
        {
            public ExpressionExtensionsException(string msg) : base(msg, null) { }
            public ExpressionExtensionsException(string msg, Exception innerException) :
                base(msg, innerException) { }
        }
        public static Expression<T> CleanInput<T>(this Expression<T> e)
        {
            return Expression.Lambda<T>(e.Body.CleanInput(), e.Parameters);
        }
        private static Expression CleanInput(this Expression e)
        {
            switch (e.NodeType)
            {
                case ExpressionType.Add:
                    {
                        var be = (BinaryExpression)e;
                        return Expression.Add(be.Left.CleanInput(), be.Right.CleanInput());
                    }
                case ExpressionType.Call:
                    {
                        var me = (MethodCallExpression)e;
                        var mi = me.Method;
                        if (string.CompareOrdinal("Pow", mi.Name) == 0)
                        {
                            var reducedParms = me.Arguments
                                .AsParallel()
                                .AsOrdered()
                                .Select(a => a.CleanInput().ReduceMore())
                                .Select(a => new { Expression = a.Item1, Changed = a.Item2 })
                                .ToArray();
                            return Expression.Power(reducedParms[0].Expression, reducedParms[1].Expression);
                        }
                        return e;
                    }
                case ExpressionType.Divide:
                    {
                        var be = (BinaryExpression)e;
                        return Expression.Divide(be.Left.CleanInput(), be.Right.CleanInput());
                    }
                case ExpressionType.Multiply:
                    {
                        var be = (BinaryExpression)e;
                        return Expression.Multiply(be.Left.CleanInput(), be.Right.CleanInput());
                    }
                case ExpressionType.Subtract:
                    {
                        var be = (BinaryExpression)e;
                        return Expression.Subtract(be.Left.CleanInput(), be.Right.CleanInput());
                    }
                default:
                    return e;
            }
        }

        public static double ContainsFactor(this Expression e, Expression factor)
        {
            factor = 
                (factor.NodeType == ExpressionType.Constant || factor.NodeType == ExpressionType.Parameter)
                ? factor
                : factor.ReduceMore().Item1;
            var be = e as BinaryExpression;

            switch (e.NodeType)
            {
                case ExpressionType.Add:
                case ExpressionType.Subtract:
                case ExpressionType.Multiply:
                case ExpressionType.Divide:
                    {
                        if (be == null)
                            throw new InvalidOperationException("Expression was of type Add, Subtract, Multiply, or Divide, but was not a BinaryExpression.");

                        var left = double.NaN;
                        var right = double.NaN;

                        Parallel.Invoke(
                            () => { left = be.Left.ContainsFactor(factor); },
                            () => { right = be.Right.ContainsFactor(factor); }
                        );
                        return Math.Min(left, right);
                    }
                case ExpressionType.Parameter:
                    {
                        if (factor.NodeType == ExpressionType.Parameter &&
                            string.CompareOrdinal(((ParameterExpression)e).Name, ((ParameterExpression)factor).Name) == 0)
                            return 1;
                        break;
                    }
                case ExpressionType.Power:
                    switch (factor.NodeType)
                    {
                        case ExpressionType.Parameter:
                            if (be == null)
                                throw new InvalidOperationException("Expression was of type Parameter, but was not a BinaryExpression.");

                            if (string.CompareOrdinal(be.Left.ToString(), ((ParameterExpression)factor).Name) == 0)
                                return 1;
                            break;
                        case ExpressionType.Power:
                            if (be == null)
                                throw new InvalidOperationException("Expression was of type Power, but was not a BinaryExpression.");

                            var fbe = (BinaryExpression)factor;
                            if (string.CompareOrdinal(be.Left.ToString(), fbe.Left.ToString()) == 0 &&
                                be.Right.NodeType == ExpressionType.Constant &&
                                fbe.Right.NodeType == ExpressionType.Constant)
                                return Math.Min(
                                    // TODO: BAD ASSUMPTION -- THESE EXPONENTS MAY NOT BE CONSTANT EXPRESSIONS
                                    Convert.ToDouble(((ConstantExpression)be.Right).Value),
                                    Convert.ToDouble(((ConstantExpression)fbe.Right).Value));
                            break;
                    }
                    break;
            }

            if (be != null)
                return be.Left.ContainsFactor(factor) *
                    ((be.Right.NodeType == ExpressionType.Constant)
                    ? Convert.ToDouble(((ConstantExpression)be.Right).Value)
                    : 1.0D);

            return 0;
        }

        public static Expression<T> Derive<T>(this Expression<T> e)
        {
            // check not null expression
            if (e == null)
                throw new ExpressionExtensionsException("Expression must be non-null");
            // check just one param (variable)
            if (e.Parameters.Count != 1)
                throw new ExpressionExtensionsException("Incorrect number of parameters");
            // check right node type (maybe not necessary)
            if (e.NodeType != ExpressionType.Lambda)
                throw new ExpressionExtensionsException("Functionality not supported");
            // calc derivative
            return Expression.Lambda<T>(e.Body.CleanInput().Derive(e.Parameters[0].Name),
                       e.Parameters);
        }

        private static IEnumerable<Expression> GetMultiplicationChainTerms(this Expression e)
        {
            var be = e as BinaryExpression;
            switch (e.NodeType)
            {
                case ExpressionType.Multiply:
                    if (be == null)
                        throw new InvalidOperationException("Expression was of type Multiply, but was not a BinaryExpression.");

                    yield return be.Left;
                    yield return be.Right;
                    //foreach (Expression eLeft in be.Left.GetMultiplicationChainTerms())
                    //    yield return eLeft;
                    //foreach (Expression eRight in be.Right.GetMultiplicationChainTerms())
                    //    yield return eRight;
                    break;
                case ExpressionType.Power:
                    if (be == null)
                        throw new InvalidOperationException("Expression was of type Multiply, but was not a BinaryExpression.");

                    if (be.Right.NodeType == ExpressionType.Constant &&
                        Math.Abs(Convert.ToDouble(((ConstantExpression)be.Right).Value) - Math.Floor(Convert.ToDouble(((ConstantExpression)be.Right).Value))) < double.Epsilon)
                        foreach (var i in Enumerable.Range(0, Convert.ToInt32(((ConstantExpression)be.Right).Value)))
                            yield return be.Left;
                    break;
                default:
                    yield return e;
                    break;
            }
        }

        private static Expression Factor(this Expression e)
        {
            switch (e.NodeType)
            {
                case ExpressionType.Add:
                    {
                        var left = ((BinaryExpression)e).Left;
                        var right = ((BinaryExpression)e).Right;
                        var gcd = GreatestCommonDenominator(left, right);
                        if (gcd != null && gcd != left && gcd != right && !(gcd.NodeType == ExpressionType.Constant && Convert.ToDouble(((ConstantExpression)gcd).Value) <= 1.0D))
                            return Expression.Multiply(
                                    Expression.Add(
                                        Expression.Divide(left, gcd),
                                        Expression.Divide(right, gcd)
                                    ), gcd);
                        return e;
                    }
                case ExpressionType.Subtract:
                    {
                        var left = ((BinaryExpression)e).Left;
                        var right = ((BinaryExpression)e).Right;
                        var gcd = GreatestCommonDenominator(left, right);
                        if (gcd != null && gcd != left && gcd != right && !(gcd.NodeType == ExpressionType.Constant && Convert.ToDouble(((ConstantExpression)gcd).Value) <= 1.0D))
                            return Expression.Multiply(
                                    Expression.Subtract(
                                        Expression.Divide(left, gcd),
                                        Expression.Divide(right, gcd)
                                    ), gcd);
                        return e;
                    }
                case ExpressionType.Divide:
                    {
                        var left = ((BinaryExpression)e).Left;
                        var right = ((BinaryExpression)e).Right;
                        var gcd = GreatestCommonDenominator(left, right);
                        if (gcd != null && gcd != left && gcd != right && !(gcd.NodeType == ExpressionType.Constant && Convert.ToDouble(((ConstantExpression)gcd).Value) <= 1.0D))
                            return Expression.Multiply(
                                    Expression.Divide(
                                        Expression.Divide(left, gcd),
                                        Expression.Divide(right, gcd)
                                    ), gcd);
                        return e;
                    }
                default:
                    return e;
            }
        }
        private static double GetCoefficient(this Expression e)
        {
            switch (e.NodeType)
            {
                case ExpressionType.Constant:
                    return Convert.ToDouble(((ConstantExpression)e).Value);
                case ExpressionType.Multiply:
                    var left = ((BinaryExpression)e).Left;
                    var right = ((BinaryExpression)e).Right;
                    if (left.NodeType == ExpressionType.Constant && right.NodeType != ExpressionType.Constant)
                        return Convert.ToDouble(((ConstantExpression)left).Value);
                    if (left.NodeType != ExpressionType.Constant && right.NodeType == ExpressionType.Constant)
                        return Convert.ToDouble(((ConstantExpression)right).Value);
                    return 1;
                default:
                    return 1;
            }
        }
        private static Expression GreatestCommonDenominator(Expression a, Expression b)
        {
            if (a == null || b == null)
                return null;

            if (a.NodeType == ExpressionType.Constant && Math.Abs(Convert.ToDouble(((ConstantExpression)a).Value)) < double.Epsilon)
                return b;
            if (b.NodeType == ExpressionType.Constant && Math.Abs(Convert.ToDouble(((ConstantExpression)b).Value)) < double.Epsilon)
                return a;

            if (a.NodeType == ExpressionType.Constant &&
                b.NodeType == ExpressionType.Constant)
            {
                var ca = Convert.ToDouble(((ConstantExpression)a).Value);
                var cb = Convert.ToDouble(((ConstantExpression)b).Value);
                if (ca > cb)
                    return GreatestCommonDenominator(Expression.Constant(ca % cb), b);

                return GreatestCommonDenominator(a, Expression.Constant(cb % ca));
            }

            if (a.NodeType == ExpressionType.Power)
                return GreatestCommonDenominator(((BinaryExpression)a).Left,
                    b.NodeType == ExpressionType.Power
                    ? ((BinaryExpression)b).Left
                    : b);
            if (b.NodeType == ExpressionType.Power)
                return GreatestCommonDenominator(((BinaryExpression)b).Left,
                    a.NodeType == ExpressionType.Power
                    ? ((BinaryExpression)a).Left
                    : a);

            if (a is BinaryExpression && a.NodeType != ExpressionType.Multiply && a.NodeType != ExpressionType.Divide)
                return GreatestCommonDenominator(GreatestCommonDenominator(((BinaryExpression)a).Left, b), ((BinaryExpression)a).Right);
            if (b is BinaryExpression && b.NodeType != ExpressionType.Multiply && b.NodeType != ExpressionType.Divide)
                return GreatestCommonDenominator(GreatestCommonDenominator(((BinaryExpression)b).Left, a), ((BinaryExpression)b).Right);

            var exp = b.GetMultiplicationChainTerms().ToArray();

            var aFactorsAlsoInB = exp
                .Select(x => new { Expression = x, ContainsFactor = a.ContainsFactor(x) })
                .Where(x => x.ContainsFactor > 0)
                .Select(x => Expression.Multiply(x.Expression, Expression.Constant(x.ContainsFactor)))
                .Concat(new Expression[] { Expression.Constant(1.0D) })
                .Aggregate(Expression.Multiply);
            if (aFactorsAlsoInB.NodeType != ExpressionType.Constant && aFactorsAlsoInB.NodeType != ExpressionType.Parameter)
                aFactorsAlsoInB = aFactorsAlsoInB.ReduceMore().Item1;
            if (aFactorsAlsoInB != null && !(aFactorsAlsoInB.NodeType == ExpressionType.Constant && Math.Abs(Convert.ToDouble(((ConstantExpression)aFactorsAlsoInB).Value) - 1.0D) < double.Epsilon)) 
                return aFactorsAlsoInB;

            exp = a.GetMultiplicationChainTerms().ToArray();

            var bFactorsAlsoInA = exp
                .Select(x => new { Expression = x, ContainsFactor = b.ContainsFactor(x) })
                .Where(x => x.ContainsFactor > 0)
                .Select(x => Expression.Multiply(x.Expression, Expression.Constant(x.ContainsFactor)))
                .Concat(new Expression[] { Expression.Constant(1.0D) })
                .Aggregate(Expression.Multiply);
            if (bFactorsAlsoInA.NodeType != ExpressionType.Constant && bFactorsAlsoInA.NodeType != ExpressionType.Parameter)
                bFactorsAlsoInA = bFactorsAlsoInA.ReduceMore().Item1;
            if (bFactorsAlsoInA != null && !(bFactorsAlsoInA.NodeType == ExpressionType.Constant && Math.Abs(Convert.ToDouble(((ConstantExpression)bFactorsAlsoInA).Value) - 1.0D) < double.Epsilon)) 
                return bFactorsAlsoInA;
            
            return null;
        }
        public static Tuple<Expression<T>, bool> ReduceCompletely<T>(this LambdaExpression le)
        {
            var x = new Tuple<Expression, bool>(le.Body, true);
            while (true)
            {
                var more = x.Item1.ReduceMore();
                if (string.CompareOrdinal(more.Item1.ToString(), x.Item1.ToString()) == 0)
                    break;
                x = more;
            }
            return new Tuple<Expression<T>, bool>(Expression.Lambda<T>(x.Item1, le.Parameters), x.Item2);
        }
        public static Tuple<Expression<T>, bool> ReduceMore<T>(this LambdaExpression le)
        {
            var bodyRet = le.Body.ReduceMore();
            return new Tuple<Expression<T>, bool>(Expression.Lambda<T>(bodyRet.Item1, le.Parameters), bodyRet.Item2);
        }
        private static Tuple<Expression, bool> ReduceMore(this Expression e)
        {
            //Console.WriteLine(e.ToString());

            switch (e.NodeType)
            {
                case ExpressionType.Lambda:
                    {
                        var le = (LambdaExpression)e;

                        var mi = typeof(StaticClass).GetMethod("ReduceMore");
                        var result = (Tuple<Expression, bool>)mi.MakeGenericMethod(new[] { le.ReturnType }).Invoke(le, null);
                        return new Tuple<Expression, bool>(result.Item1, result.Item2);
                    }
                case ExpressionType.Add:
                    {
                        var left = ((BinaryExpression)e).Left;
                        var right = ((BinaryExpression)e).Right;

                        if (left.NodeType == ExpressionType.Constant && double.Equals(((ConstantExpression)left).Value, 0.0))
                            return new Tuple<Expression, bool>(right.ReduceMore().Item1, true);
                        if (right.NodeType == ExpressionType.Constant && double.Equals(((ConstantExpression)right).Value, 0.0))
                            return new Tuple<Expression, bool>(left.ReduceMore().Item1, true);
                        if (left.NodeType == ExpressionType.Constant &&
                            right.NodeType == ExpressionType.Constant)
                            return new Tuple<Expression, bool>(Expression.Constant(Convert.ToDouble(((ConstantExpression)left).Value) + Convert.ToDouble(((ConstantExpression)right).Value)), true);

                        // SPECIAL CASE: If both left and right are same, convert to a multiplication
                        if (string.CompareOrdinal(left.ToString(), right.ToString()) == 0)
                            return new Tuple<Expression, bool>(Expression.Multiply(Expression.Constant(2.0D), left), true);

                        Tuple<Expression, bool> leftR = null;
                        Tuple<Expression, bool> rightR = null;

                        Parallel.Invoke(
                            () =>
                            {
                                leftR =
                                    (left.NodeType == ExpressionType.Constant || left.NodeType == ExpressionType.Parameter)
                                    ? new Tuple<Expression, bool>(left, false)
                                    : left.ReduceMore().Item1.Factor().ReduceMore();
                            },
                            () =>
                            {
                                rightR =
                                    (right.NodeType == ExpressionType.Constant || right.NodeType == ExpressionType.Parameter)
                                    ? new Tuple<Expression, bool>(right, false)
                                    : right.ReduceMore().Item1.Factor().ReduceMore();
                            }
                            );

                        if (leftR.Item2 || rightR.Item2)
                            return new Tuple<Expression, bool>(Expression.Add(leftR.Item1, rightR.Item1), true);

                        return new Tuple<Expression, bool>(Expression.Add(leftR.Item1, rightR.Item1), false);
                    }
                case ExpressionType.Subtract:
                    {
                        var left = ((BinaryExpression)e).Left;
                        var right = ((BinaryExpression)e).Right;

                        // X - 0 = X
                        if (right.NodeType == ExpressionType.Constant && double.Equals(((ConstantExpression)right).Value, 0.0))
                            return left.ReduceMore();

                        // 0 - K = -K
                        if (left.NodeType == ExpressionType.Constant && Math.Abs(Convert.ToDouble(((ConstantExpression)left).Value)) < double.Epsilon && right.NodeType == ExpressionType.Constant)
                            return new Tuple<Expression, bool>(Expression.Constant(Convert.ToDouble(((ConstantExpression)right).Value) * -1.0D), true);

                        if (left.NodeType == ExpressionType.Constant &&
                            right.NodeType == ExpressionType.Constant)
                            return new Tuple<Expression, bool>(Expression.Constant(Convert.ToDouble(((ConstantExpression)left).Value) - Convert.ToDouble(((ConstantExpression)right).Value)), true);

                        // If (A+B)-A then just B, also if (A+B)-B just A
                        if (left.NodeType == ExpressionType.Add &&
                            string.CompareOrdinal(((BinaryExpression)left).Left.ToString(), right.ToString()) == 0)
                            return new Tuple<Expression, bool>(((BinaryExpression)left).Right, true);
                        if (left.NodeType == ExpressionType.Add &&
                            string.CompareOrdinal(((BinaryExpression)left).Right.ToString(), right.ToString()) == 0)
                            return new Tuple<Expression, bool>(((BinaryExpression)left).Left, true);

                        // If A-(A+B) then just -B, also if B-(A+B) then just -A
                        if (right.NodeType == ExpressionType.Add &&
                            string.CompareOrdinal(left.ToString(), ((BinaryExpression)right).Left.ToString()) == 0)
                            return new Tuple<Expression, bool>(Expression.Multiply(Expression.Constant(-1.0D), ((BinaryExpression)right).Right), true);
                        if (right.NodeType == ExpressionType.Add &&
                            string.CompareOrdinal(left.ToString(), ((BinaryExpression)right).Right.ToString()) == 0)
                            return new Tuple<Expression, bool>(Expression.Multiply(Expression.Constant(-1.0D), ((BinaryExpression)right).Left), true);

                        // Minus a negative is plus a positive
                        if (right.NodeType == ExpressionType.Constant &&
                            Convert.ToDouble(((ConstantExpression)right).Value) < 0)
                            return new Tuple<Expression, bool>(Expression.Add(left, Expression.Constant(Math.Abs(Convert.ToDouble(((ConstantExpression)right).Value)))), true);
                        if (right.NodeType == ExpressionType.Multiply &&
                            ((BinaryExpression)right).Left.NodeType == ExpressionType.Constant &&
                            Convert.ToDouble(((ConstantExpression)((BinaryExpression)right).Left).Value) < 0)
                            return new Tuple<Expression, bool>(Expression.Add(left, Expression.Multiply(
                                Expression.Constant(Math.Abs(Convert.ToDouble(((ConstantExpression)((BinaryExpression)right).Left).Value))),
                                ((BinaryExpression)right).Right)), true);
                        if (right.NodeType == ExpressionType.Multiply &&
                           ((BinaryExpression)right).Right.NodeType == ExpressionType.Constant &&
                           Convert.ToDouble(((ConstantExpression)((BinaryExpression)right).Right).Value) < 0)
                            return new Tuple<Expression, bool>(Expression.Add(left, Expression.Multiply(
                                ((BinaryExpression)right).Left,
                                Expression.Constant(Math.Abs(Convert.ToDouble(((ConstantExpression)((BinaryExpression)right).Right).Value))))), true);

                        Tuple<Expression, bool> leftR = null;
                        Tuple<Expression, bool> rightR = null;

                        Parallel.Invoke(() =>
                                        {
                                            leftR =
                                                (left.NodeType == ExpressionType.Constant || left.NodeType == ExpressionType.Parameter)
                                                ? new Tuple<Expression, bool>(left, false)
                                                : left.ReduceMore().Item1.Factor().ReduceMore();
                                        },
                                        () =>
                                        {
                                            rightR =
                                                (right.NodeType == ExpressionType.Constant || right.NodeType == ExpressionType.Parameter)
                                                ? new Tuple<Expression, bool>(right, false)
                                                : right.ReduceMore().Item1.Factor().ReduceMore();
                                        });

                        if (leftR.Item2 || rightR.Item2)
                            return new Tuple<Expression, bool>(Expression.Subtract(leftR.Item1, rightR.Item1), true);
                        return new Tuple<Expression, bool>(Expression.Subtract(leftR.Item1, rightR.Item1), false);
                    }
                case ExpressionType.Multiply:
                    {
                        var left = ((BinaryExpression)e).Left;
                        var right = ((BinaryExpression)e).Right;

                        if (left.NodeType == ExpressionType.Constant && double.Equals(Convert.ToDouble(((ConstantExpression)left).Value), 0.0))
                            return new Tuple<Expression, bool>(Expression.Constant(0.0), true);
                        if (right.NodeType == ExpressionType.Constant && double.Equals(Convert.ToDouble(((ConstantExpression)right).Value), 0.0))
                            return new Tuple<Expression, bool>(Expression.Constant(0.0), true);
                        if (left.NodeType == ExpressionType.Constant && double.Equals(Convert.ToDouble(((ConstantExpression)left).Value), 1.0))
                            return new Tuple<Expression, bool>((right.NodeType == ExpressionType.Constant || right.NodeType == ExpressionType.Parameter)
                                ? right
                                : right.ReduceMore().Item1, true);
                        if (right.NodeType == ExpressionType.Constant && double.Equals(Convert.ToDouble(((ConstantExpression)right).Value), 1.0))
                            return new Tuple<Expression, bool>(
                                (left.NodeType == ExpressionType.Constant || left.NodeType == ExpressionType.Parameter)
                                ? left
                                : left.ReduceMore().Item1, true);
                        if (left.NodeType == ExpressionType.Constant && right.NodeType == ExpressionType.Constant)
                            return new Tuple<Expression, bool>(Expression.Constant(Convert.ToDouble(((ConstantExpression)left).Value) * Convert.ToDouble(((ConstantExpression)right).Value)), true);

                        // Fast path.. can't reduce (K * X)
                        if (left.NodeType == ExpressionType.Constant && right.NodeType == ExpressionType.Parameter)
                            return new Tuple<Expression, bool>(e, false);
                        if (right.NodeType == ExpressionType.Constant && left.NodeType == ExpressionType.Parameter)
                            return new Tuple<Expression,bool>(e, false);

                        // (K * (K' * X)) -> ((K * K') * X)
                        if (left.NodeType == ExpressionType.Constant &&
                            right.NodeType == ExpressionType.Multiply &&
                            ((BinaryExpression)right).Left.NodeType == ExpressionType.Constant)
                            return new Tuple<Expression, bool>(
                                Expression.Multiply(
                                    Expression.Constant(Convert.ToDouble(((ConstantExpression)left).Value) * Convert.ToDouble(((ConstantExpression)((BinaryExpression)right).Left).Value)),
                                    ((BinaryExpression)right).Right), true);

                        // (K * (X * K')) -> ((K * K') * X)
                        if (left.NodeType == ExpressionType.Constant &&
                            right.NodeType == ExpressionType.Multiply &&
                            ((BinaryExpression)right).Right.NodeType == ExpressionType.Constant)
                            return new Tuple<Expression, bool>(
                                Expression.Multiply(
                                    Expression.Constant(Convert.ToDouble(((ConstantExpression)left).Value) * Convert.ToDouble(((ConstantExpression)((BinaryExpression)right).Right).Value)),
                                    ((BinaryExpression)right).Left), true);

                        // ((K' * X) * K) -> ((K * K') * X)
                        if (right.NodeType == ExpressionType.Constant &&
                            left.NodeType == ExpressionType.Multiply &&
                            ((BinaryExpression)left).Left.NodeType == ExpressionType.Constant)
                            return new Tuple<Expression, bool>(
                                Expression.Multiply(
                                    Expression.Constant(Convert.ToDouble(((ConstantExpression)right).Value) * Convert.ToDouble(((ConstantExpression)((BinaryExpression)left).Left).Value)),
                                    ((BinaryExpression)left).Right), true);

                        // ((X * K') * K) -> ((K * K') * X)
                        if (right.NodeType == ExpressionType.Constant &&
                            left.NodeType == ExpressionType.Multiply &&
                            ((BinaryExpression)left).Right.NodeType == ExpressionType.Constant)
                            return new Tuple<Expression, bool>(
                                Expression.Multiply(
                                    Expression.Constant(Convert.ToDouble(((ConstantExpression)right).Value) * Convert.ToDouble(((ConstantExpression)((BinaryExpression)left).Right).Value)),
                                    ((BinaryExpression)left).Left), true);

                        // If I am a multiply, and one side of me is a divide which in term has a numerator of 1, then refactor it into a division
                        // A * (1 / B) = A / B
                        if (right.NodeType == ExpressionType.Divide &&
                            ((BinaryExpression)right).Left.NodeType == ExpressionType.Constant &&
                            Math.Abs(Convert.ToDouble(((ConstantExpression)((BinaryExpression)right).Left).Value) - 1.0D) < double.Epsilon)
                        {
                            return new Tuple<Expression, bool>(Expression.Divide(left, ((BinaryExpression)right).Right), true);
                        }
                        if (left.NodeType == ExpressionType.Divide &&
                            ((BinaryExpression)left).Left.NodeType == ExpressionType.Constant &&
                            Math.Abs(Convert.ToDouble(((ConstantExpression)((BinaryExpression)left).Left).Value) - 1.0D) < double.Epsilon)
                        {
                            return new Tuple<Expression, bool>(Expression.Divide(right, ((BinaryExpression)left).Right), true);
                        }

                        // SPECIAL CASE, whole term removal.
                        // Is the either side a divide, and if so, if the right hand term of the divide is the same as my other term, only keep the other.
                        // i.e.  (B / A) x A = B  or  (A x (B / A)) == B)
                        if (left.NodeType == ExpressionType.Divide &&
                            string.CompareOrdinal(((BinaryExpression)left).Right.ToString(), right.ToString()) == 0)
                            return new Tuple<Expression, bool>(((BinaryExpression)left).Left.ReduceMore().Item1, true);
                        if (right.NodeType == ExpressionType.Divide &&
                            string.CompareOrdinal(((BinaryExpression)right).Right.ToString(), left.ToString()) == 0)
                            return new Tuple<Expression, bool>(((BinaryExpression)right).Left.ReduceMore().Item1, true);

                        Tuple<Expression, bool> leftR = null;
                        Tuple<Expression, bool> rightR = null;

                        Parallel.Invoke(
                            () =>
                            {
                                leftR =
                                    (left.NodeType == ExpressionType.Constant || left.NodeType == ExpressionType.Parameter)
                                    ? new Tuple<Expression, bool>(left, false)
                                    : left.ReduceMore().Item1.Factor().ReduceMore();
                            },
                            () =>
                            {
                                rightR =
                                    (right.NodeType == ExpressionType.Constant || right.NodeType == ExpressionType.Parameter)
                                    ? new Tuple<Expression, bool>(right, false)
                                    : right.ReduceMore().Item1.Factor().ReduceMore();
                            }
                            );

                        if (leftR.Item2 || rightR.Item2)
                            return new Tuple<Expression, bool>(Expression.Multiply(leftR.Item1, rightR.Item1), true);

                        return new Tuple<Expression, bool>(Expression.Multiply(leftR.Item1, rightR.Item1), false);
                    }
                case ExpressionType.Divide:
                    {
                        var left = ((BinaryExpression)e).Left;
                        var right = ((BinaryExpression)e).Right;

                        // SPECIAL CASE, divide by one.
                        if (right.NodeType == ExpressionType.Constant && double.Equals(Convert.ToDouble(((ConstantExpression)right).Value), 1.0))
                            return new Tuple<Expression, bool>(left.ReduceMore().Item1, true);

                        // SPECIAL CASE, divided by itself
                        if (string.CompareOrdinal(left.ToString(), right.ToString()) == 0)
                            return new Tuple<Expression, bool>(Expression.Constant(1.0D), true);

                        // SPECIAL CASE, whole term removal.
                        // Is the left hand side a multiply, and if so, are one of the two terms my term?  If so, only keep the other.
                        // (A*B)/A = B  and  (A*B)/B = A
                        if (left.NodeType == ExpressionType.Multiply &&
                            string.CompareOrdinal(((BinaryExpression)left).Left.ToString(), right.ToString()) == 0)
                            return new Tuple<Expression, bool>(((BinaryExpression)left).Right.ReduceMore().Item1, true);
                        if (left.NodeType == ExpressionType.Multiply &&
                            string.CompareOrdinal(((BinaryExpression)left).Right.ToString(), right.ToString()) == 0)
                            return new Tuple<Expression, bool>(((BinaryExpression)left).Left.ReduceMore().Item1, true);

                        // SPECIAL CASE, if the numerator is a power, and the term that is raised is my denominator, reduce power by one.
                        // (A ^ B) / A -> (A ^ (B - 1))
                        if (left.NodeType == ExpressionType.Power &&
                            string.CompareOrdinal(((BinaryExpression)left).Left.ToString(), right.ToString()) == 0)
                            return new Tuple<Expression, bool>(Expression.Power(((BinaryExpression)left).Left, Expression.Subtract(((BinaryExpression)left).Right, Expression.Constant(-1.0D))), true);

                        // SPECIAL CASE (Y/(X^N))/X = Y/(X^(N+1))
                        if (left.NodeType == ExpressionType.Divide &&
                            ((BinaryExpression)left).Right.NodeType == ExpressionType.Power &&
                            string.CompareOrdinal(((BinaryExpression)((BinaryExpression)left).Right).Left.ToString(), right.ToString()) == 0)
                            return new Tuple<Expression, bool>(Expression.Divide(((BinaryExpression)left).Left,
                                Expression.Power(right, Expression.Add(((BinaryExpression)((BinaryExpression)left).Right).Right, Expression.Constant(1.0D)))), true);

                        // SPECIAL CASE, if my left-right is also my right, divide by the power of 2 instead of using two divisions
                        // (Y/X)/X = Y/(X^2)
                        if (left.NodeType == ExpressionType.Divide &&
                            string.CompareOrdinal(((BinaryExpression)left).Right.ToString(), right.ToString()) == 0)
                            return new Tuple<Expression, bool>(Expression.Divide(((BinaryExpression)left).Left, Expression.Power(right, Expression.Constant(2.0D))), true);

                        // Are any of my denominator's multiplication chain terms in my nominator's multiplication chain?  If so, factor them out.
                        var leftCopy = left;
                        var rightCopy = right;
                        {
                            var virgin = true;
                            var numeratorTerms = leftCopy.GetMultiplicationChainTerms().ToArray();
                            foreach (var denominatorTerm in rightCopy.GetMultiplicationChainTerms())
                            {
                                var match = numeratorTerms
                                    .Select(n => GreatestCommonDenominator(n, denominatorTerm))
                                    .FirstOrDefault(gcf => gcf != null);
                                if (match == null)
                                    break;

                                virgin = false;
                                leftCopy = Expression.Divide(leftCopy, match);//.ReduceMore().Item1;
                                rightCopy = Expression.Divide(rightCopy, match);//.ReduceMore().Item1;
                            }
                            if (!virgin)
                                return new Tuple<Expression, bool>(Expression.Divide(leftCopy, rightCopy), true);
                        }

                        // SeanM: Symbolic division is going to be fun!  Pow/Pow, Mul/Pow, Pow/Mul, Mul/Mul, Expression/Term...

                        Tuple<Expression, bool> leftR = null;
                        Tuple<Expression, bool> rightR = null;

                        Parallel.Invoke(
                            () =>
                            {
                                leftR =
                                    (left.NodeType == ExpressionType.Constant || left.NodeType == ExpressionType.Parameter)
                                    ? new Tuple<Expression, bool>(left, false)
                                    : left.ReduceMore().Item1.Factor().ReduceMore();
                            },
                            () =>
                            {
                                rightR =
                                    (right.NodeType == ExpressionType.Constant || right.NodeType == ExpressionType.Parameter)
                                    ? new Tuple<Expression, bool>(right, false)
                                    : right.ReduceMore().Item1.Factor().ReduceMore();
                            }
                            );

                        if (leftR.Item2 || rightR.Item2)
                            return new Tuple<Expression, bool>(Expression.Divide(leftR.Item1, rightR.Item1), true);
                        return new Tuple<Expression, bool>(Expression.Divide(leftR.Item1, rightR.Item1), false);
                    }
                case ExpressionType.Power:
                    {
                        var left = ((BinaryExpression)e).Left;
                        var right = ((BinaryExpression)e).Right;

                        // SPECIAL CASE: To the power of 1 is itself
                        if (right.NodeType == ExpressionType.Constant &&
                            double.Equals(Convert.ToDouble(((ConstantExpression)right).Value), 1.0))
                            return new Tuple<Expression, bool>(
                                (left.NodeType == ExpressionType.Constant || left.NodeType == ExpressionType.Parameter)
                                ? left
                                : left.ReduceMore().Item1, true);
                        // SPECIAL CASE: To the power of 0 is one
                        if (right.NodeType == ExpressionType.Constant &&
                            double.Equals(Convert.ToDouble(((ConstantExpression)right).Value), 0.0))
                            return new Tuple<Expression, bool>(Expression.Constant(1.0D), true);
                        // SPECIAL CASE: If the left side is also a power, multiply powers to reduce
                        if (left.NodeType == ExpressionType.Power)
                            return new Tuple<Expression, bool>(Expression.Power(
                                ((BinaryExpression)left).Left,
                                Expression.Multiply(((BinaryExpression)left).Right, right)), true);

                        Tuple<Expression, bool> leftR = null;
                        Tuple<Expression, bool> rightR = null;

                        Parallel.Invoke(
                             () =>
                             {
                                 leftR =
                                     (left.NodeType == ExpressionType.Constant || left.NodeType == ExpressionType.Parameter)
                                     ? new Tuple<Expression, bool>(left, false)
                                     : left.ReduceMore().Item1.Factor().ReduceMore();
                             },
                             () =>
                             {
                                 rightR =
                                     (right.NodeType == ExpressionType.Constant || right.NodeType == ExpressionType.Parameter)
                                     ? new Tuple<Expression, bool>(right, false)
                                     : right.ReduceMore().Item1.Factor().ReduceMore();
                             }
                             );

                        if (leftR.Item2 || rightR.Item2)
                            return new Tuple<Expression, bool>(Expression.Power(leftR.Item1, rightR.Item1).ReduceMore().Item1, true);

                        return new Tuple<Expression, bool>(Expression.Power(leftR.Item1, rightR.Item1), false);
                    }
                case ExpressionType.Call:
                    {
                        var me = (MethodCallExpression)e;
                        var mi = me.Method;

                        // TEMPORARY
                        // method should be static and its class - Math
                        if (!mi.IsStatic || (mi.DeclaringType != null && mi.DeclaringType.FullName != "System.Math"))
                            throw new ExpressionExtensionsException("Not implemented function: " +
                                mi.DeclaringType + "/" + mi.Name);

                        var reducedParms = me.Arguments
                            .AsParallel()
                            .AsOrdered()
                            .Select(a => (a.NodeType == ExpressionType.Constant || a.NodeType == ExpressionType.Parameter)
                                ? new Tuple<Expression, bool>(a, false)
                                : a.ReduceMore())
                            .Select(a => new { Expression = a.Item1, Changed = a.Item2 });

                        switch (mi.Name)
                        {
                            default:
                                return new Tuple<Expression, bool>(Expression.Call(me.Object, me.Method, reducedParms.Select(a => a.Expression)), reducedParms.Any(a => a.Changed));
                        }
                    }
                case ExpressionType.Constant:
                case ExpressionType.Parameter:
                    {
                        return new Tuple<Expression, bool>(e, false);
                    }
                default:
                    return new Tuple<Expression, bool>(e, false);
            }
        }
        public static Tuple<Expression<T>, bool> SimplifyCompletely<T>(this LambdaExpression le)
        {
            var x = new Tuple<Expression, bool>(le.Body, true);
            while (true)
            {
                var more = x.Item1.SimplifyMore();
                if (string.CompareOrdinal(more.Item1.ToString(), x.Item1.ToString()) == 0)
                    break;
                x = more;
            }
            return new Tuple<Expression<T>, bool>(Expression.Lambda<T>(x.Item1, le.Parameters), x.Item2);
        }
        private static Tuple<Expression, bool> SimplifyMore(this Expression e)
        {
            switch (e.NodeType)
            {
                case ExpressionType.Multiply:
                    // Distribute coefficients into other terms. A * (B [+-] C) = A * B [+-] A * C
                    Func<Expression, BinaryExpression, Expression> coeffDistribute = (a, bc) => {
                        switch (bc.NodeType)
                        {
                            case ExpressionType.Add:
                                return Expression.Add(Expression.Multiply(a, bc.Left), Expression.Multiply(a, bc.Right));
                            case ExpressionType.Subtract:
                                return Expression.Subtract(Expression.Multiply(a, bc.Left), Expression.Multiply(a, bc.Right));
                            case ExpressionType.Divide: // Odd: A * (B / C) = (A * B) / C
                                return Expression.Divide(Expression.Multiply(a, bc.Left), bc.Right);
                        }
                        return null;
                    };
                    var be = (BinaryExpression)e;
                    Expression result = null;
                    if (be.Left is BinaryExpression && !(be.Right is BinaryExpression))
                        result = coeffDistribute(be.Right, (BinaryExpression)be.Left);
                    else if (be.Right is BinaryExpression && !(be.Left is BinaryExpression))
                        result = coeffDistribute(be.Left, (BinaryExpression)be.Right);
                    else if (be.Left is BinaryExpression && be.Right is BinaryExpression) // Distributive property
                    { 
                        // (A + B)(C + D) = A(C + D) + B(C + D)   OR  
                        // (A + B)(C - D) = A(C - D) + B(C - D)   OR  
                        // (A - B)(C - D) = A(C - D) - B(C - D)   OR  
                        // (A - B)(C + D) = A(C + D) - B(C + D)

                        if (be.Left.NodeType == ExpressionType.Add && (be.Right.NodeType == ExpressionType.Add || be.Right.NodeType == ExpressionType.Subtract))
                            result = Expression.Add(coeffDistribute(((BinaryExpression)be.Left).Left, (BinaryExpression)be.Right), coeffDistribute(((BinaryExpression)be.Left).Right, (BinaryExpression)be.Right));
                        if (be.Left.NodeType == ExpressionType.Subtract && (be.Right.NodeType == ExpressionType.Add || be.Right.NodeType == ExpressionType.Subtract))
                            result = Expression.Subtract(coeffDistribute(((BinaryExpression)be.Left).Left, (BinaryExpression)be.Right), coeffDistribute(((BinaryExpression)be.Left).Right, (BinaryExpression)be.Right));
                    }
                    if (result != null)
                        return new Tuple<Expression, bool>(result, true);
                    


                    // K * (K' * X) = (K * K') * X
                    // If I am a multiply, and one side of me is constant, and the other is a multiply which in turns
                    // has one side as a constant and one side as a .Call or .Parameter node, then distribute my coefficient into the inner term.
                    if (EitherOr((BinaryExpression)e, ExpressionType.Constant, ExpressionType.Multiply) &&
                        (EitherOr((BinaryExpression)EitherOr((BinaryExpression)e, ExpressionType.Constant, ExpressionType.Multiply, ExpressionType.Multiply), ExpressionType.Constant, ExpressionType.Call)))
                    {
                        var coeff = Convert.ToDouble(((ConstantExpression)EitherOr((BinaryExpression)e, ExpressionType.Constant, ExpressionType.Multiply, ExpressionType.Constant)).Value);
                        var innerCoeff = Convert.ToDouble(((ConstantExpression)EitherOr((BinaryExpression)EitherOr((BinaryExpression)e, ExpressionType.Constant, ExpressionType.Multiply, ExpressionType.Multiply), ExpressionType.Constant, ExpressionType.Call, ExpressionType.Constant)).Value);
                        return new Tuple<Expression, bool>(
                            Expression.Multiply(
                            Expression.Constant(coeff * innerCoeff),
                            EitherOr((BinaryExpression)EitherOr((BinaryExpression)e, ExpressionType.Constant, ExpressionType.Multiply, ExpressionType.Multiply), ExpressionType.Constant, ExpressionType.Call, ExpressionType.Call)), true);
                    }
                    if (EitherOr((BinaryExpression)e, ExpressionType.Constant, ExpressionType.Multiply) &&
                        (EitherOr((BinaryExpression)EitherOr((BinaryExpression)e, ExpressionType.Constant, ExpressionType.Multiply, ExpressionType.Multiply), ExpressionType.Constant, ExpressionType.Parameter)))
                    {
                        var coeff = Convert.ToDouble(((ConstantExpression)EitherOr((BinaryExpression)e, ExpressionType.Constant, ExpressionType.Multiply, ExpressionType.Constant)).Value);
                        var innerCoeff = Convert.ToDouble(((ConstantExpression)EitherOr((BinaryExpression)EitherOr((BinaryExpression)e, ExpressionType.Constant, ExpressionType.Multiply, ExpressionType.Multiply), ExpressionType.Constant, ExpressionType.Parameter, ExpressionType.Constant)).Value);
                        return new Tuple<Expression, bool>(
                            Expression.Multiply(
                            Expression.Constant(coeff * innerCoeff),
                            EitherOr((BinaryExpression)EitherOr((BinaryExpression)e, ExpressionType.Constant, ExpressionType.Multiply, ExpressionType.Multiply), ExpressionType.Constant, ExpressionType.Parameter, ExpressionType.Parameter)), true);
                    }

                    return new Tuple<Expression, bool>(e, false);
                default:
                    return new Tuple<Expression,bool>(e, false);
            }
        }



        private static Expression Derive(this Expression e, string paramName)
        {
            switch (e.NodeType)
            {
                // constant rule
                case ExpressionType.Constant:
                    return Expression.Constant(0.0);
                // parameter
                case ExpressionType.Parameter:
                    if (((ParameterExpression)e).Name == paramName)
                        return Expression.Constant(1.0);

                    return Expression.Constant(0.0);
                // sign change
                case ExpressionType.Negate:
                    var op = ((UnaryExpression)e).Operand;
                    return Expression.Negate(op.Derive(paramName));
                // sum rule
                case ExpressionType.Add:
                    {
                        var dleft = ((BinaryExpression)e).Left.Derive(paramName).ReduceMore().Item1;
                        var dright = ((BinaryExpression)e).Right.Derive(paramName).ReduceMore().Item1;
                        return Expression.Add(dleft, dright);
                    }
                // sum rule, subtracted
                case ExpressionType.Subtract:
                    {
                        var dleft = ((BinaryExpression)e).Left.Derive(paramName);
                        var dright = ((BinaryExpression)e).Right.Derive(paramName);
                        return Expression.Subtract(dleft, dright);
                    }
                // product rule
                case ExpressionType.Multiply:
                    {
                        var left = ((BinaryExpression)e).Left;
                        var right = ((BinaryExpression)e).Right;
                        var dleft = left.Derive(paramName);
                        var dright = right.Derive(paramName);
                        return Expression.Add(
                                Expression.Multiply(dleft, right),
                                Expression.Multiply(left, dright));
                    }
                // quotent rule
                case ExpressionType.Divide:
                    {
                        var left = ((BinaryExpression)e).Left;
                        var right = ((BinaryExpression)e).Right;
                        var dleft = left.Derive(paramName);
                        var dright = right.Derive(paramName);
                        return Expression.Divide(Expression.Subtract(Expression.Multiply(dleft, right), Expression.Multiply(left, dright)), Expression.Power(right, Expression.Constant(2.0D)));
                    }
                // power rule
                case ExpressionType.Power:
                    {
                        var left = ((BinaryExpression)e).Left;
                        var right = ((BinaryExpression)e).Right;

                        return Expression.Multiply(right, Expression.Power(left, Expression.Subtract(right, Expression.Constant(1.0D))));
                    }
                case ExpressionType.Call:
                    Expression e1;
                    var me = (MethodCallExpression)e;
                    var mi = me.Method;

                    // TEMPORARY
                    // method should be static and its class - Math
                    if (!mi.IsStatic || (mi.DeclaringType != null && mi.DeclaringType.FullName != "System.Math"))
                        throw new ExpressionExtensionsException("Not implemented function: " +
                            mi.DeclaringType + "/" + mi.Name);

                    var parms = me.Arguments;
                    switch (mi.Name)
                    {
                        case "Log":
                            // der natural log of x is 1 / x
                            if (parms.Count == 1) // Natural log
                                e1 = Expression.Divide(Expression.Constant(1.0D), parms[0]);
                            else
                                e1 = Expression.Divide(Expression.Constant(1.0D), Expression.Multiply(parms[0], Expression.Call(typeof(Math).GetMethod("Log"), parms[1])));
                            break;
                        case "Log10":
                            e1 = Expression.Divide(Expression.Constant(1.0D), Expression.Multiply(parms[0], Expression.Call(typeof(Math).GetMethod("Log"), Expression.Constant(10.0D))));
                            break;                       
                        case "Sin":
                            // The derivative of Sin is Cos
                            e1 = Expression.Call(typeof(Math).GetMethod("Cos"), parms[0]);
                            break;
                        case "Cos":
                            // The derivative of Cos is -Sin
                            e1 = Expression.Multiply(
                                Expression.Constant(-1.0D),
                                Expression.Call(typeof(Math).GetMethod("Sin"), parms[0]));
                            break;
                        case "Tan":
                            // The derivative of Tan is the secent squared 
                            e1 = Expression.Divide(
                                Expression.Constant(1.0D),
                                Expression.Power(
                                    Expression.Call(typeof(Math).GetMethod("Cos"), parms[0]),
                                    Expression.Constant(2.0D)));
                            break;
                        default:
                            throw new ExpressionExtensionsException("Not implemented function: " +
                                mi.Name);
                    }

                    // chain rule
                    return Expression.Multiply(e1, parms[0].Derive(paramName));
                // *** other node types here ***
                default:
                    throw new ExpressionExtensionsException("Not implemented expression type: " + e.NodeType.ToString());
            }
        }
        private static bool EitherOr(BinaryExpression e, ExpressionType t1, ExpressionType t2)
        {
            return (e.Left.NodeType == t1 && e.Right.NodeType == t2) ||
                   (e.Left.NodeType == t2 && e.Right.NodeType == t1);
        }
        private static Expression EitherOr(BinaryExpression e, ExpressionType t1, ExpressionType t2, ExpressionType targetType)
        {
            if (targetType != t1 && targetType != t2)
                throw new ArgumentException("Target type is not one of the two search types");
            if (t1 == t2)
                throw new ArgumentException("Both search types cannot be the same since the target type would match both");

            if ((e.Left.NodeType == t1 && e.Right.NodeType == t2) || (e.Left.NodeType == t2 && e.Right.NodeType == t1))
            {
                return (e.Left.NodeType == targetType) ? e.Left : e.Right;
            }
            return null;
        }

        public static string WritePretty(this Expression e)
        {
            var s = e.WritePretty2();

            s = Regex.Replace(s, @"\((\d+) \* \(([A-Za-z0-9]+) \^ ([A-Za-z0-9]+)\)\)", "$1$2^$3");
            s = Regex.Replace(s, @"(\d+) \* \(([A-Za-z0-9]+) \^ ([A-Za-z0-9]+)\)", "$1$2^$3");
            s = Regex.Replace(s, @"\(([A-Za-z0-9]+) \^ ([A-Za-z0-9]+)\)", "$1^$2");

            s = Regex.Replace(s, @"\((?<a>.+) (?<op>[+-/*]) (?<b>.+)\) \k<op> (?<c>.+)", "(${a} ${op} ${b} ${op} ${c})");
            
            if (s.StartsWith("f(x) = (") && s.EndsWith(")"))
                s = "f(x) = " + s.Substring(8, s.Length - 9);

            if (s.StartsWith("(") && s.EndsWith(")"))
                s = s.Substring(1, s.Length - 2);

            return s;
        }

        private static string WritePretty2(this Expression e)
        {
            e = e.CleanInput();

            switch (e.NodeType)
            {
                case ExpressionType.Add:
                    {
                        var be = (BinaryExpression)e;

                        if (be.Left.NodeType == ExpressionType.Add)
                            return string.Format("{0} + {1}", be.Left.WritePretty2(), be.Right.WritePretty2());

                        return string.Format("({0} + {1})", be.Left.WritePretty2(), be.Right.WritePretty2());
                    }
                case ExpressionType.Call:
                    {
                        var me = (MethodCallExpression)e;
                        var mi = me.Method;
                        if (!mi.IsStatic || (mi.DeclaringType != null && mi.DeclaringType.FullName != "System.Math"))
                            throw new ExpressionExtensionsException("Not implemented function: " +
                                mi.DeclaringType + "/" + mi.Name);

                        var parms = me.Arguments;
                        switch (mi.Name)
                        {
                            case "Cos":
                                return string.Format("cos({0})", parms[0].WritePretty2());
                            case "Log":
                                if (me.Arguments.Count == 1)
                                    return string.Format("ln({0})", parms[0].WritePretty2());
                                return string.Format("log[{0}]({1})", parms[1].WritePretty2(), parms[0].WritePretty2());
                            case "Log10":
                                return string.Format("log({0})", parms[0].WritePretty2());
                            case "Sin":
                                return string.Format("sin({0})", parms[0].WritePretty2());
                            case "Tan":
                                return string.Format("tan({0})", parms[0].WritePretty2());
                            default:
                                throw new ExpressionExtensionsException("Not implemented function: " + mi.Name);
                        }
                    }
                case ExpressionType.Divide:
                    {
                        var be = (BinaryExpression)e;
                        return string.Format("({0} / {1})", be.Left.WritePretty2(), be.Right.WritePretty2());
                    }
                case ExpressionType.Lambda:
                    {
                        var le = (LambdaExpression)e;
                        return string.Format("f({0}) = {1}", le.Parameters.First(), le.Body.WritePretty2());
                    }
                case ExpressionType.Multiply:
                    {
                        var be = (BinaryExpression)e;
                        if (be.Left.NodeType == ExpressionType.Parameter && be.Right.NodeType == ExpressionType.Constant)
                            return string.Format("{0}{1}", be.Right.WritePretty2(), be.Left.WritePretty2());
                        if (be.Left.NodeType == ExpressionType.Constant && be.Right.NodeType == ExpressionType.Parameter)
                            return string.Format("{0}{1}", be.Left.WritePretty2(), be.Right.WritePretty2());

                        return string.Format("({0} * {1})", be.Left.WritePretty2(), be.Right.WritePretty2());
                    }
                case ExpressionType.Power:
                    {
                        var be = (BinaryExpression)e;
                        if (be.Right.NodeType == ExpressionType.Parameter)
                            return string.Format("{0}^{1})", be.Left.WritePretty2(), be.Right.WritePretty2());
                        return string.Format("({0} ^ {1})", be.Left.WritePretty2(), be.Right.WritePretty2());
                    }
                case ExpressionType.Subtract:
                    {
                        var be = (BinaryExpression)e;
                        return string.Format("({0} - {1})", be.Left.WritePretty2(), be.Right.WritePretty2());
                    }
                default:
                    return e.ToString();
            }
        }
    }
}