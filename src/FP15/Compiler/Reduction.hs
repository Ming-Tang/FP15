-- | Module for converting expressions to different representations, and
-- emitting errors along the way.
--
-- This module re-exports symbols from the submodules.
module FP15.Compiler.Reduction (

-- * @ExprAST -> BExpr@
  BError(..)
, ResolvedOp(..)
, convExprAST

-- * @Expr@
, resolveExpr

-- * @BaseExpr@
, BaseExprError(..)
, toBaseExpr

) where
import FP15.Compiler.Reduction.BExpr
import FP15.Compiler.Reduction.Resolution
import FP15.Compiler.Reduction.BaseExpr
