Soloution
=========

Parsing text to LINQ-based functional expressions to perform calculus derivatives.

An example usage of the included console application:


`Enter an equation in the format: f(x) = ...`

`x^3+2x^2+4`

`I read: x^3+2x^2+4`

`Reduced: f(x) = x^3 + 2x^2 + 4`

`Derive f(x) = x^3 + 2x^2 + 4`

`Reduced: f(x) = 3x^2 + 4x`


The final line (3x^2 + 4x) is derivative of the original function (x^3 + 2x^2 + 4), using the Power Rule.
This calculation is a three-step process:

1. Parse the text input into Expression instances from the System.Linq.Expressions namespace
2. Perform derivations based on recursive functions that operate on expressions
3. Serialize the resulting lambda function to a string representation for final output.
