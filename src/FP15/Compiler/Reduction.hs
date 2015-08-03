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
, convBExpr

-- * @Expr@
, resolveExpr
) where
import FP15.Compiler.Reduction.BExpr
import FP15.Types(BExpr(..), Expr(..))
import Control.Applicative((<$>))

convBExpr :: BExpr -> Either a Expr
convBExpr (BConst x) = return $ Const x
convBExpr (BApp f xs) = App f <$> mapM convBExpr xs
convBExpr (BFunc f) = return $ Func f
convBExpr (BLet _ _) = error "convBExpr: BLet"

resolveExpr :: Expr -> Either a Expr
resolveExpr e = undefined
