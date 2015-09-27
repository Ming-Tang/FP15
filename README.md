FP15
====
FP15 is a strict, dynamically-typed function-level programming language based on
the [FP][FP] programming language created by John Backus.

Some major departures from the original FP are (1) left-to-right reading, (2) no
explicit constant function and (3) user-defined functionals.

Introduction
============
In function-level programming (also called [point-free][pointfree] and
[tacit][tacit]), functions are defined without mentioning any of their
arguments.

Here is the factorial function in FP15:

```fp15
fac = (=0) : 1 | (* (-1) fac)
```

Here is a function that finds Pythagorean triples upto a number:

```fp15
pytr = [(1..), (1..), (1..2*)] cross ?(#0^2 + #1^2 = #2^2)
```

Example:
```
> 15 pytr
[[3, 4, 5], [4, 3, 5], [5, 12, 13], [6, 8, 10], [8, 6, 10],
 [8, 15, 17], [9, 12, 15], [12, 5, 13], [12, 9, 15], [15, 8, 17]]
```

Setup
=====
You can provision the Vagrant machine.

```
# Build
cabal sandbox init
cabal install --only-dependencies
cabal configure
cabal build

# Run REPL
./fp15-repl.py

# Run a file (using the main function)
cabal run < example.fp
```

Tutorial
========
See [Tutorial.md](./Tutorial.md).

[FP]: https://en.wikipedia.org/wiki/FP_%28programming_language%29
[pointfree]: https://wiki.haskell.org/Pointfree
[tacit]: https://en.wikipedia.org/wiki/Tacit_programming

Task List
=========

Language Features
=================
 - [X] Empty list items
 - [X] Smart spitting
 - [X] Infix notation
 - [X] Comma notation
 - [ ] Unicode escapes in string literals
 - [ ] User-defined operator definitions
 - [ ] User-defined ternary operator support
 - [ ] Recursive block
 - [ ] Local definitions
 - [ ] User-defined functionals

Runtime
=======
 - [ ] Embed multiple compilation phases within Expr
 - [ ] Context stack
 - [ ] I/O support
 - [ ] Reference cells and arrays
 - [ ] Actual higher-order functions
 - [ ] Uniqueness typing enforcement

Code
====
 - [X] Refactor name resolution so absolute and relative names are distinguhshed
