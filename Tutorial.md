FP15 Tutorial
=============
In FP15, functions are defined using functionals: Combining forms that create
complex functions from constants and simpler functions. The most used
functionals are `Compose`, `Fork` and `If`.

Example Code
============

Factorial
---------

```fp15
fac = (= 0) : 1 | (* (-1) fac)
```

The definition of `fac` is recursive. The top-level is a conditional that checks
if the input is zero. In FP15, a conditional looks like `p : f | g` where `p` is
the predicate, and `f` and `g` are the branches to take (all functions). The
conditional works by applying the argument on `p`, and if it is true, the result
is the same argument applied on `f`, or `g` otherwise.

In the true branch, there is a constant `1`. Unlike the original FP, there is no
special marker for constants.

The false branch contains an infix notation (a special syntax signaled by `()`).
The expression reads as "times minus one factorial", where the subject of
"times" and "minus one" is the input argument.

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

The `sum` function computes the sum of a list. The aliases of `sum` are `add`
and the operator `+`. With smart splitting, the definition of `inner` can be
written as `trans @*+`.

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

Quicksort
---------

```fp15
qs = (len > 1) : decons [?<{>} qs, [#0], ?<{<=} qs] ++
```

Basic Syntax
============
The syntax for function definition is simply `name = expr`. Definitions are
separated by semicolons or newlines.

A FP15 program consists of multiple definitions, and the entry point is the
`main` function.

Line comments begin with `--`.

To define a function in the REPL (`./fp15-repl.py`), prefix the definition with
`:d`. To show definitions, enter `:s`, and to delete a line of definition, enter
`:e ` with the line number to delete.

Values
======
The datatypes in FP15 are boolean (`#f`, `#t`), character (`#\a`, `#\b`, etc.),
integer (`123`), real number (`123.456`, `6.02e23`), symbol (`'sym`), string
(`"hello"`) and list `[1, 2, 3]`.

The tilde (`~`) prefix is used to negate a number. For example, negative five is
`~5`.

Values occuring in FP15 programs are constant functions (everything is a
function in FP15). For example, `"test"` is a function that disregards its
argument and returns the string `"test"`.

Function Composition
====================
Function composition is denoted by the juxtaposing functions in sequence.
Function composition is performed from left to right. For example, in Haskell
you would write `c . b . a`, and in FP15, it is `a b c`, and it means "apply a
on the argument, then apply `b`, then apply `c`".

Lists and Forks
===============
Lists are constructed using forks. A fork is denoted by a comma-separated list
of functions `[f1, ... , fn]`, and a fork works by taking an argument `x`, and
creates a list of `n` items `[yi, ... , yn]` where `yi` equals `fi` of `x`.

```
x [f1, ..., fn] => [x f1, ..., x fn]
```

All functions in FP15 take one argument. Multiple arguments are passed using
lists. For example, the function `f(x) = x^2 + 3 x` is written as:
```
[[_, 2] ^, [3, _] *] +
```
`_` is the identity function. Identity functions in a comma-separated list can
be omitted with empty items:
```
[[_, 2] ^, [3, _] *] +  =>  [[,2] ^, [3,] *] +
[_, a, _, _, b, _]      =>  [, a,,, b,]
```

Indexers are functions that access an element of a list by index. An indexer is
denoted by `#n` where `n` is a nonnegative integer (`#0`, `#1` and so on).

Here is an example:
```
>>> ["x", "y", "z"] #1
"y"
```

Indexers are tremendously useful for writing multi-parameter functions. For
example, `f(x) = cos(x^2 + y^2) / x` is written as:

```
f = [[[#0, 2] ^, [#1, 2] ^] + cos, #0] %
```

With fork and composition, more complex expressions can be built. To get used to
FP15, it is important to practice translating arithmetic expressions to FP15. It
is easier to perform the conversion from outer subexpressions first.

```
-- % is the division operator

-- f(x) = 2 x + 1/x
f = [[2,] *, [1,] %] +

-- g(x, y) = (x + y) / (1 + x y)
g = [+, [1, *] +] %

-- h(x, y, z) = sin(x + y(z^2 - 1)^3) / sqrt(x (z + 1) - x y + z^2)
h = [[#0, [[#1, [[#2, 2] ^, 1] -] *, 3] ^] + sin,
     [[[#0, [#2, 1] +] *, [#0, #1] *] -, [#2, 2]^] + sqrt] %
```

Conditionals
============

 - basics of if
 - factorial and fibonacci
 - grouping

Infix Notation
==============

List Processing
===============

 - map and filter
 - trans and cross
 - insert

`distl`, `distr` and Distributing Functionals
=============================================

 - the problem of closures
 - how distl and distr work
 - distributing functionals: MapL, MapR, etc.
