FP15 Tutorial
=============
In FP15, functions are defined using functionals: Combining forms that create
complex functions from constants and simpler functions. The most used
functionals are `Compose`, `Fork` and `If`.

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



Absolute Value
--------------

```fp15
abs_ = (< 0) : ~ | _
```

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
