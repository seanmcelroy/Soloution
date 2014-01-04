using System;
using System.Diagnostics;
using System.Linq.Expressions;
using System.Text.RegularExpressions;

namespace Soloution
{
    class Program
    {
        static void Main()
        {
            //Console.WriteLine("Derive 2x^4 + 3x - 6");

            //Expression<Func<double, double>> exp = (x) => 2 * Math.Pow(x, 4) + 3 * x - 6;
            //Expression<Func<double, double>> exp = (x) => (Math.Pow(x, 2D) + 3D * x) * Math.Log(x);
            Expression<Func<double, double>> exp = x => (2 / x) + (-3 / (Math.Pow(x, 2))) + (6 * x);

            while (exp != null)
            {
                Console.WriteLine("Enter an equation in the format: f(x) = ...");
                var eq = Console.ReadLine();
                exp = ParseExpression(eq);
                Console.WriteLine("I read: {0}", eq);
                //Console.WriteLine("As: {0}", exp.WritePretty());
                //Console.WriteLine("Cleaned: {0}", exp.CleanInput().WritePretty());
                Console.WriteLine("Reduced: {0}", exp.CleanInput().ReduceCompletely<Func<double, double>>().Item1.WritePretty());

                Console.WriteLine();
                Console.WriteLine("Derive {0}", exp.CleanInput().ReduceCompletely<Func<double, double>>().Item1.WritePretty());

                var der = exp.Derive().ReduceCompletely<Func<double, double>>().Item1;
                Console.WriteLine("Reduced: {0}", der.WritePretty());

                exp = der;
                Console.ReadLine();
            }
        }

        private static Expression<Func<double, double>> ParseExpression(string expression)
        {
            expression = expression.Replace(" ", "");

            // Exponent
            expression = Regex.Replace(expression, @"([^\(\)+-/\*\^]+\^[^\(\)+-/\*\^]+)", "($1)");

            expression = Regex.Replace(expression, @"(\d+)x\^(\d+|\([^\)]+\))", "$1*(x^$2)");
            expression = Regex.Replace(expression, @"(\d+)x[^\^]", "($1*x)");

            // MDAS
            expression = Regex.Replace(expression, @"([^\(])([^\(\)+-/\*\^]+[+-/\*]\([^\)]+\))([^\)])", "$1($2)$3");
            expression = Regex.Replace(expression, @"[^\(]([^\(\)+-/\*\^]+[+-/\*][^\(\)+-/\*\^]+)[^\)]", "($1)");

            expression = "0+(" + expression + ")";

            return Expression.Lambda<Func<double, double>>(ParseExpressionRecursive(expression).Item1, Expression.Parameter(typeof(double), "x"));
        }

        private static Tuple<Expression, int> ParseExpressionRecursive(string inner, Expression preExistingLeft = null)
        {
            if (string.IsNullOrWhiteSpace(inner))
                throw new ArgumentNullException(inner);

            // (a + (b + c) - (d * (f ^ g)))
            // 1    2     1   2    3     210
            // PEMDAS
            // ((a + (b + c)) - (d * (f ^ g)))
            // 12    3     21   2    3     210

            var left = preExistingLeft;
            var operation = default(ExpressionType?);
            
            for (var pos = 0; pos < inner.Length; pos++)
            {
                Expression newExpression = null;

                switch (inner[pos])
                {
                    case '(':
                        // We have to figure out how deep to go...
                        int pCount = 0, matchingPos = -1;
                        for (var ip = pos; ip < inner.Length; ip++)
                        {
                            if (inner[ip] == '(')
                                pCount++;
                            else if (inner[ip] == ')')
                            {
                                pCount--;
                                if (pCount == 0)
                                {
                                    matchingPos = ip;
                                    break;
                                }
                            }
                        }
                        Debug.Assert(matchingPos != -1);
                        var result = ParseExpressionRecursive(inner.Substring(pos + 1, matchingPos - pos - 1));
                        newExpression = result.Item1;
                        inner = inner.Substring(result.Item2 + 2);

                        if (left == null)
                            left = newExpression;
                        else
                        {
                            switch (operation)
                            {
                                case ExpressionType.Add:
                                    if (inner.Length - (matchingPos - pos - 1) > 1)
                                        return ParseExpressionRecursive(inner.Substring(matchingPos - pos - 1), Expression.Add(left, newExpression));

                                    return new Tuple<Expression, int>(Expression.Add(left, newExpression), pos);
                                case ExpressionType.Divide:
                                    return new Tuple<Expression, int>(Expression.Divide(left, newExpression), pos);
                                case ExpressionType.Multiply:
                                    return new Tuple<Expression, int>(Expression.Multiply(left, newExpression), pos);
                                case ExpressionType.Subtract:
                                    return new Tuple<Expression, int>(Expression.Subtract(left, newExpression), pos);
                                case ExpressionType.Power:
                                    return new Tuple<Expression, int>(Expression.Power(left, newExpression), pos);
                                default:
                                    throw new InvalidOperationException();
                            }
                        }
                        break;
                    case ')':
                        throw new InvalidOperationException();
                    case '+':
                        operation = ExpressionType.Add;
                        break;
                    case '/':
                        operation = ExpressionType.Divide;
                        break;
                    case '*':
                        operation = ExpressionType.Multiply;
                        break;
                    case '-':
                        operation = ExpressionType.Subtract;
                        break;
                    case '^':
                        operation = ExpressionType.Power;
                        break;
                    default:
                    {
                        var match = Regex.Match(inner.Substring(pos), @"^((([0-9]{1,3}),)(([0-9]{3}),)*[0-9]{3}|[0-9]+)?(\.[0-9]*)?", RegexOptions.ExplicitCapture);
                        if (match.Success && match.Value.Length > 0)
                            newExpression = Expression.Constant(double.Parse(match.Value, System.Globalization.NumberStyles.Number));
                        else
                        {
                            match = Regex.Match(inner.Substring(pos), @"^[A-Za-z]{1}", RegexOptions.ExplicitCapture);
                            if (match.Success && match.Value.Length > 0)
                                newExpression = Expression.Parameter(typeof(double), match.Value);
                        }

                        if (newExpression == null)
                            throw new InvalidOperationException();

                        pos += match.Length - 1;

                        if (left == null)
                            left = newExpression;
                        else
                            switch (operation)
                            {
                                case ExpressionType.Add:
                                    return new Tuple<Expression, int>(Expression.Add(left, newExpression), pos);
                                case ExpressionType.Divide:
                                    return new Tuple<Expression, int>(Expression.Divide(left, newExpression), pos);
                                case ExpressionType.Multiply:
                                    return new Tuple<Expression, int>(Expression.Multiply(left, newExpression), pos);
                                case ExpressionType.Subtract:
                                    return new Tuple<Expression, int>(Expression.Subtract(left, newExpression), pos);
                                case ExpressionType.Power:
                                    return new Tuple<Expression, int>(Expression.Power(left, newExpression), pos);
                                default:
                                    throw new InvalidOperationException();
                            }

                        break;
                    }
                }
            }

            throw new InvalidOperationException();
        }
    }
}