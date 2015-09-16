-- | Module for converting expressions to different representations, and
-- emitting errors along the way.
--
-- This module re-exports symbols from the submodules.
module FP15.Compiler.Reduction (

-- * @ExprAST -> BExpr@
  BError(..)
, ResolvedOp(..)
, convExprAST

-- * @BExpr -> Expr@
, liftBExpr

-- * @Expr@
, resolveExpr

-- * @BaseExpr@
, BaseExprError(..)
, toBaseExpr

) where
import FP15.Types
import FP15.Compiler.Reduction.BExpr
import FP15.Compiler.Reduction.Resolution
import FP15.Compiler.Reduction.BaseExpr

-- | The 'liftBExpr' function converts an 'BExpr' to an 'Expr' and a set of
-- lifted declarations.
liftBExpr :: BExpr -> Either () Expr
liftBExpr = return . mapEx undefined
