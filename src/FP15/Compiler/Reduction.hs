{-# LANGUAGE ViewPatterns #-}
-- | Module for converting expressions to different representations, and
-- emitting errors along the way.
module FP15.Compiler.Reduction (

-- * @ExprAST -> BExpr@
  BError(..)
, ResolvedOp(..)
, convExprAST

) where
import FP15.Compiler.Reduction.BExpr
