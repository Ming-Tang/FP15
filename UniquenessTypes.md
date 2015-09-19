Uniqueness Types
================

FP15 performs I/O and operate on mutable storage through uniqueness types. The
idea behind uniqueness type is that a value can be updated in-place because
there is only one reference to it. The advantage of uniqueness typing is that we
can mathematically reason about an I/O function as if it's a pure function.

I/O and the `RealWorld`
=======================

In FP15, I/O operations are performed through the `RealWorld` object. A function
that performs I/O takes the `RealWorld` as an argument, and returns the updated
`RealWorld`. The older `RealWorld` no longer exists and a runtime error happens
when the older `RealWorld` is being used.

Here is a cat program (repeats the input) in FP15:

```
main = readLn printLn main
```

The `readLn` function takes the `RealWorld`, and reads a line of input, and
returns a pair of updated `RealWorld` and the input line. Then, the result is
sent to the `printLn` function, which takes a pair of `RealWorld` and the line
to output, and returns the updated `RealWorld`.

All I/O functions in FP15 will take the `RealWorld` as the first argument (or
`RealWorld` alone), and return the `RealWorld` as first element of a list (or
`RealWorld` alone).

Here is an improper use of the `RealWorld`:

```
improper = readLn [{_, ("Hello, "+++)} printLn,
                   {_, ("Goodbye, "+++)} printLn]
```

The mathematical interpretation of the function `improper` is that it takes a
`RealWorld`, and then gets a pair of (input string, updated `RealWorld`), and
then returns two different `RealWorld`s where the first element has the
`RealWorld` after printing `"Hello, "` plus the input, and the second element is
another `RealWorld` (parallel universe) after printing `"Goodbye, "` plus the
input.

This program will crash right before the goodbye is being printed because a
uniqueness violation was detected at that point.

Reference Cells
===============

A reference cell is a unique reference to a value that can be updated in-place.

The `ref` function creates a unique reference of the specified value, and the
`(Update f)` functional updates a reference cell in-place with function `f`
applied on its value.

The `update` function takes a reference cell and a value, updates the reference
cell with the value, and returns the updated reference cell.

The `deref` function takes a reference cell, and returns a pair of (the
reference cell, its value).

Arrays
======

Mutable arrays are crucial for performance.  In fact, quicksort depends on
mutability for efficiency.

In FP15, mutable arrays use the same uniqueness typing system for I/O and
reference cells. Mutable arrays are non-resizeable, and can be
multi-dimensional. Multi-dimensional arrays must be rectangular, i.e., with same
number of elements in each dimension and the array's size can be express as a
product of lengths.

Runtime Enforcement
===================

FP15 is a dynamically-typed language in which type errors, including uniqueness
violations happen at runtime.

Everything that are unique typed are wrapped as a unique reference, which means
an integer `5` is different from a unique reference to an integer `5`. A unique
reference also has a revision ID attached to it, which is a pair of (revision
counter, GUID). The revision ID is attached to both beside the unique value in
memory and the unique reference. Before an operation is performed on the unique
value, the unique reference's revision ID is checked against the unique value's
reference ID. If there is a mismatch, then we detected a uniqueness violation.
Otherwise, we perform the uniqueness operation, and update and synchronize the
revision ID by incrementing the revision counter and generating a new GUID.

Design Considerations
=====================

The problem with having separate reference types for uniqueness typing is the
overhead of invoking uniqueness functions explicitly. For example, one does not
simply `Map` a unique list, but the programmer needs to wrap it in `Update`.

In some static typed functional programming langauges with uniqueness typing,
such as Clean and the langauge in ["Uniqueness Typing Simplified"](uts), all
functions, even `map`, `const` and `fst` participate in uniqueness typing with
uniqueness type inference and even polymorphic uniqueness. The problem with this
approach is that for a function, changing the implementation will change the
uniqueness characteristics (and won't be clearly revealed without static
typing).

Also, in some uniqueness I/O systems, files belong to a separate uniqueness type
`*File`. This limits side effects to happen on a specific file only but that
does not mean rest of the real world won't be affected. (For example, programs
that watch a file for changes will perform further side effects) This is the
reason there is a monolithic `RealWorld` object rather than a fine-grained
`File` unique type.

Relations to Other Programming Languages
========================================

Haskell
-------
In Haskell, the `IO` monad captures real-world side effects and the `ST` monad
allows a programmer to write imperative code (with mutable memory) with pure
behaviour (same input = same output). In FP15, the `RealWorld` has the same role
of `I/O` and unique references take the role of Haskell's `ST` monad.

However, in [Haskell Core](core), there is a `RealWorld` object used to
represent `IO` actions.

Linear Lisp
-----------
[Linear Lisp](linear-lisp) is a Lisp Machine where all cons cells have reference
count of 1. Unlike FP15, *everything* in Linear Lisp is under uniqueness typing.
In addition, while Linear Lisp's goal is to enable memory management without
garbage collection, FP15's goal is to provide I/O and mutable references in a
pure functional way.

C++ and Rust
------------
In C++11, the `std::unique_ptr` behaves like unique-typed reference cells in
FP15. `std::unique_ptr` enforces uniqueness with `std::move` and `std::swap`.
The compiler cannot ensure that a stale (moved away) `unique_ptr` cannot be
dereferenced.

In Rust, the compiler ensures mutable references (`&mut`) are never borrowed
twice, making it impossible to shoot yourself in the foot.

In FP15, uniqueness typing is latent (information about uniqueness type is
attached to value) and runtime-enforced, so the programmer knows when a
uniqueness type is used incorrectly.

  [core]: http://www.haskellforall.com/2012/10/hello-core.html
  [linear-lisp]: http://www.pipeline.com/~hbaker1/LinearLisp.html
  [uts]: http://www.edsko.net/pubs/ifl07-paper.pdf
