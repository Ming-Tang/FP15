Multi Phase Expression
======================

Notes about refactoring expression compilation phases.

Old Phases
==========

1. Undesugared `ExprAST`
2. Desugared `BExpr`
3. Resolved `RExpr`

New Phases
==========

Later phases embed exprs in earlier phases: `RExpr` embeds `BExpr` embeds
`ExprAST`. Module context (a lookup object) is attached to an expression itself.

```haskell
data AnyLookup = forall a. Lookup a => AnyLookup a
data NExpr
  = NConst Value
  | NF AFName              -- ^ a function (absolute function name)
  | NApp AFlName [NExpr]   -- ^ a functional form (absolute functional name)
  | NRead Int              -- ^ get nth context variable
  | NPush NExpr            -- ^ push context variable
  | NBExpr AnyLookup BExpr

data BExpr
  = BConst Value
  | BF RFName              -- ^ a function (relative function name)
  | BApp RFlName [BExpr]   -- ^ a functional (relative functional name)
  | BWith AnyLookup BExpr  -- ^ with module
  | BExprAST ExprAST

```
