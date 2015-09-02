FP15 Tutorial
=============
In FP15, functions are defined using functionals: Combining forms that create
complex functions from constants and simpler functions. The most used
functionals are `Compose`, `Fork` and `If`.

Basic Syntax
============
The syntax for function definition is simply `name = expr`. Definitions are
separated by semicolons or newlines.

A FP15 program consists of multiple definitions, and the entry point is the
`main` function.

Line comments begin with `--`.

Values
------
The datatypes in FP15 are boolean (`#f`, `#t`), character (`#\a`, `#\b`, etc.),
integer (`123`), real number (`123.456`, `6.02e23`), symbol (`'sym`), string
(`"hello"`) and list `[1, 2, 3]`.

The tilde (`~`) prefix is used to negate a number. For example, negative five is
`~5`.

Values occuring in FP15 programs are constant functions (everything is a
function in FP15). For example, `"test"` is a function that disregards its
argument and returns the string `"test"`.

Identifiers
-----------
All identifiers are made of underscores, digits and letters, and identifiers
cannot start with a digit. There are two kinds of identifiers: function and
functional. Functions begin with a lowercase letter and functionals begin with
an uppercase letter. If an identifier begins with underscores, then the first
letter determines its kind. If an identifier consists only underscores, then it
is a function. An identifier cannot start with underscores then a digit.

Here are some valid functions:

```
_
___
_test
_x
_x1
a1b2_
gt
sort
x1
```

Here are some valid functionals:

```
F1
Fork
If
_X1__
__Test
```

Operators
---------
Operators are composed of characters `@ ! ? ~ % ^ & * - + = < > / \ : |`. For a
cluster of operator characters like `@<<=`, the boundary between operators are
not well-defined until the operators defined in the current scope are known. See
"Smart Splitting" below.

Off-Side Rule
-------------
FP15 has offside rule, which means definitions outside of brackets can be
continued with an increased indentation. Line breaks separate definitions, but
they can be joined with the semicolon.

The following two programs are equivalent:

```
g = a b c
  d e f
h = x y z
```

```
g = a b c d e f; h = x y z
```

Using the REPL
--------------
To define a function in the REPL (`./fp15-repl.py`), prefix the definition with
`:d `. To show definitions, enter `:s`, and to delete a line of definition,
enter `:e ` with the line number to delete.

FP15 By Example
===============

Average
-------

```fp15
avg = [sum, len] %
```

The `avg` function demonstrates FP15's two of the basis combinators: `Fork` and
`Compose`. The average of a list is the sum divided by its length. How to
construct this program in FP15? In FP15, the `%` function takes the quotient of
two arguments (more precisely, a two-element list), so the result of the `avg`
function must be `%`.

What are the arguments for `%`? To feed the sum and length into `%`, we need to
put a function before it. In FP15, function composition is read left-to-right,
with no operator between them. In Haskell, you would write `f . g . h` and in
FP15, you would write `h g f` to do the same thing.

The function `[sum, len]` before the `%` produces a list where the first element
is the sum of the list and the second element is the length of the list. In
FP15, the functional for achieving that is the fork, and its syntax is a
comma-separated list in square brackets. Fork works by taking an argument `x`,
and producing an list where the `i`th element is `x` applied to the `i`th
function in the fork. Here is an example of fork that calculates the sum,
product, minimumm, maximum and length of a list.

```
[1, 2, 5, 8] [sum, prod, min, max, len]
=> [[1, 2, 5, 8] sum, [1, 2, 5, 8] prod,
    [1, 2, 5, 8] min, [1, 2, 5, 8] max, [1, 2, 5, 8] len]
=> [16, 80, 1, 8, 4]
```

### With Infix Notation

```fp15
avg = (sum % len)
```

The expression `[sum, len] %` can be rewritten as `(sum % len)`, using a
syntactic sugar called the infix notation. The infix notation is what you expect
from other programming languages, except the operands are functions. FP15's
infix notation is user-customizable, and it supports prefix operators infix
operators of left and right associativities.

Harmonic Mean
-------------

```fp15
reci = [1, _] %
hm = @reci avg reci
```

The harmonic mean of a list of numbers is the reciprocal of the arithmetic mean
of the reciprocals. The `hm` function works by applying reciprocal to each item
of the list (`@reci`), then computing the mean of them (`avg` defined in the
previous example), and computes the reciprocal again (`reci`).

The new syntax introduced in this example is the `@` operator, which represents
the `Map` functional. The `Map` functional takes a function, and produces a
function that takes a list of any length, and applies the function on all
elements of the list.

The function `reci` takes the reciprocal of a number. Notice the first element
in the fork is a number `1`. It is technically a constant function, a function
that takes an argument, ignores it and returns a value. The second element, `_`,
is the identity function that takes an argument and returns it unchanged.

### With Infix Notation

```fp15
reci = (1 %)
hm = @reci avg reci
```

The `reci` function can be rewritten in terms of the infix notation: `(1 % _)`,
but when operands are `_`, they can be omitted altogether, leaving `(1 %)`.
Operand elision is based on Haskell's infix sections (like `(2 +)` or `(* 3)`),
but is more powerful: Operands can be missing in any part of the infix
expression, for example, expressions such as `(* 2 + 3)` (double and add three),
`(* - 2^)` (`x*x + 2^x` as a function of x), are possible in FP15 but not in
Haskell's sections.

Absolute Value
--------------

```fp15
abs_ = (< 0) : ~ | _
```

The absolute value demonstrates the conditional form, `cond : iftrue | iffalse`.
The conditional form takes an argument `x`, applies `x cond`, if it is true,
then the result is `x iftrue`, otherwise, `x iffalse`.

`(< 0)` checks if a number is less than zero and `~` negates a number.

Factorial
---------

```fp15
fac = (= 0) : 1 | (* (-1) fac)
```

Fibonacci
---------

```fp15
fib = (< 2) : 1 | ((-1) fib + (-2) fib)
```

Inner Product
-------------

```fp15
inner = trans @* sum
```

The function takes a two-element (also works with three or more) list of
vectors, and computes the inner product of them. It is a composition of three
functions, `trans`, `@*` and `sum`.

The first function, `trans`, transposes a two-dimensional list, so that all
vector components are paired up. For example, `[[1,2,3], [7,8,9]] trans` equals
`[[1,7], [2,8], [3,9]]`.

The second function, `@*`, computes the product for each pairs. `@*` is actually
a compound function created from the `Map` functional and the times operator
`*`. `@*` are two consecutive symbols, how does the compiler understand they are
separate? The compiler will split a cluster of symbols based on the existing
operators in the scope, and because the operator `@*` does not exist, the
compiler splits it into `@` and `*`.

Pythagorean Triples
-------------------

```fp15
pytr = [(1..), (1..), (1..2*)] cross ?(#0^2 + #1^2 = #2^2)
```

The `pytr` function has three parts executed from left to right.

The first part, `[(1..), (1..), (1..2*)]`, creates three ranges, where the first
and second ranges, `(1..)`, are integers from 1 to the input argument `n`. The
third range, `(1..2*)`, creates a range counting upto `2n` instead. These
expressions are examples of the infix notation with missing operands, a powerful
syntactic sugar in FP15. The missing operand works like sections (like `(2 +)`)
in Haskell, but more powerful.

The cross function creates a list of all combinations of items picked from the
input lists. It is FP15's equivalent of nested loops.

Finally, the expression `?(#0^2 + #1^2 = #2^2)` filters out combinations that
conform to the equality `x^2 + y^2 = z^2`, where `x, y, z` are the three
elements of the input item, accessed by `#0, #1, #2` respectively.

### With Cross Operator

```fp15
pytr = (1.. >< 1.. >< 1..2*) ?(#0^2 + #1^2 = #2^2)
```

The `cross` function has infix counterpart, `><`, and its precedence is lower
than `..`.

### With Comma Notation

```fp15
pytr = (1.. >< 1.. >< 1..2*) ?(^2 +, ^2 =, ^2)
```
The comma notation is a syntactic abstraction that reduces the character count
of multivariable functions. The idea is that many multivariable functions are
infix expressions with indexers (`#0`, `#1`, ...) in the beginning (first item
of function composition) of their operands, and the commas (`,`) increment or
decrement the indexers.

The comma notation is a tricky syntax. Let's walk through how the comma
notation above, `(^2 +, ^2 =, ^2)`, is desugared.

First, the expression is divided into regions on operators. Commas around
operators are part of the operators. In the diagram below, operands (even
elided) are marked with `{ }` and operators are marked with `|`.

```
   { } | { } |  { } | { }  |  { } | { }
 (     ^  2  +,     ^  2   =,     ^  2  )
```

Then, we walk through the expression from left to right with a counter starting
at zero. The current value of the counter is marked on the operands, and the
counter will change with on operators with commas around them. When there are
`n` commas after an operator (`+,`), the counter increments by `n`, and when
there are `n` commas before (`,+`) an operator, the counter decrements by `n`.
In the diagram below, the values of the counters are marked inside `{ }`.

```
   {0} | {0} |  {1} | {1}  |  {2} | {2}
 (     ^  2  +,     ^  2   =,     ^  2  )
```

There is a rule of comma notation not applied in this example. If the values
ever goes below zero, they are incremented by the same amount so that the
smallest value is zero (`1 0 -3 -2 1 2` becomes `4 3 0 1 4 5`).

Finally, the indexers of the operand's counter value are inserted before the
operands. The commas are removed and this completes the conversion from comma
notation to regular infix notation. Indexers might be inserted before constants
but this does not affect the final result.

```
(#0 ^ #0 2 + #1 ^ #1 2 = #2 ^ #1 2)
```

Quicksort
---------

```fp15
qs = (len > 1) : decons [?<> qs, [#0], ?<<= qs] ++ | _
```

This is the quicksort function often used to introduce functional programming.
It is actually inefficient because of the time complexity of linked list
manipulation. The English translation of the function above is:

If the length of the list is greater than one (`(len > 1)`), then (`:`):
 - Split the list into head and tail (`decons`), then (`[`):
     - find all elements of the tail less than the head (`?<>`), and sort it
       recursively (`qs`), and (`,`)
     - wrap the head as a singleton `[#0]`, and (`,`) ".
     - find all elements of the tail greater than the head (`?<<=`), and sort it
       recursively (`qs`). (`]`)
 - and join them (`++`).
Otherwise, return the list itself.

### Smart Splitting
The trickiest parts of the `qs` function are `?<>` and `?<<=`. They are actually
composed of two operators: `?<>` is made of the functional `?<` and the function
`>` and `?<<=` is composed of `?<` and `<=`. The compiler splits them apart
using the feature called smart splitting. Smart splitting splits a cluster of
symbols into meaningful operators based on the operators available in the scope.
Smart splitting is greedy, which means it maximizes the length of the leftmost
operators. This means `?<>` is splitted into `?<` and `>` not `?` and `<>`, even
though `<>` is a valid operator.

Without smart splitting, `?<>` can be written as `?< <`, using spaces, or
`?<{<}`, using grouping braces.

### Distributing Functions
`?<` is like `?`, which filters a list, but `?<` has an extra feature to deal
with the fact that closures don't exist in FP15. Let's translate the `qs`
function literally to Haskell:

```
qs [] = []
qs [x] = [x]
qs (x:xs) = qs (filter (\y -> x > y)) ++ [x] ++ qs (filter (\y -> x <= y))
```

Notice in the lambda functions in the two occurences of `filter`, the variable
`x` is passed as a closure. Closures don't exist in FP15 because there is no
currying nor named arguments. The solution to this problem is to distribute the
closed value into the list. For example, we want to filter a list by comparing
against an external variable . For example, we have the input `[x, ys]` and we
would like to filter the list `ys` by `x > y` for each element `y`. In Haskell,
this would be `filter (\y -> x > y)`.

In FP15, we use the `dlstl` function to distribute the value `x` into each
elements of the list. The definition of distl is:

```
[x, [y1, ..., yn]] distl = [[x, y1], ..., [x, yn]]
```

For example, `[1, [2, 3, 4]] distl => [[1,2], [1,3], [1,4]]`.

Then we perform the filtering in the distributed list (`?>`), and finally remove
the distributed element (`@#1`), making the overall function `distl ?> @#1`.

Since this pattern is so common, we have an operator to shorten that expression,
which is `?<`. `?<` is defined as `?<p = distl ?<p @#1`.

There is also a right-hand version of `distl`, `distr`. The definition of `distr`
is:

```
[[x1, ..., xn], y] => [[x1, y], ..., [xn, y]]
```

and right-hand version of `?<`, which is `?>`. There are distributed
counterparts for mapping as well. `@<` is defined as `@<f = distl @f` and `@>`
is `@>f = distr @f`.

### Operators are Aliases
In the definition of `qs`, `>`, `<`, `<=` and `++` are operators being used
outside of infix expressions. When operators are being used outside of infix
expressions, they are not subject to the syntax of infix notation. There is
another rule of FP15 stating that all operators are aliases of something. The
operator `>` is an alias of function `gt`, `<` is `lt`, `<=` is `le`, and `++`
is `append`. Outside of infix expressions, the operator itself and its alias can
be used interchangeably. The definition of `qs` can be rewritten as:

```fp15
qs = (len > 1) : decons [?<gt qs, [#0], ?<le qs] append | _
```

What about `?<`? `?<` is a prefix operator that maps to a functional. That
means, you cannot use it inside `()` because `()` is reserved for function
operators (including operators `>`, `<`, `<=`, `++` above). Additionally,
functional operators such as `?<` maps to functionals as well. `?<` is also
called `FilterL`. The syntax for using functionals by name is `(F f1 ... fn)`.
`qs` can be written as:

```fp15
qs = (len > 1) : decons [(FilterL gt) qs, [#0], (FilterL le) qs] append | _
```

### Functionals

In fact, all the syntaxes that combine functions together, such as the if, fork,
and compose, have their named functionals as well:

```fp15
qs = (If (Compose (Fork len 1) gt)
         (Compose
           decons
           (Fork
             (Compose (FilterL gt) qs)
             (Fork #0)
             (Compose (FilterL le) qs))
           append)
         _)
```

The FP15 compiles works by translating all syntactic sugars such as the comma
notation, infix notation, if, fork and compose, into expressions resembling
S-expressions like the one above. Then the S-expressions are fed into
translation backends, which translate into a target language, such as Scheme.
