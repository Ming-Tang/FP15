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
) where
import FP15.Compiler.Reduction.BExpr
import FP15.Types(BExpr(..), Expr(..))
import Control.Applicative((<$>))

-- | The 'liftBExpr' function converts an 'BExpr' to an 'Expr' and a set of
-- lifted declarations.
liftBExpr :: BExpr -> Either a Expr
liftBExpr (BConst x) = return $ Const x
liftBExpr (BApp f xs) = App f <$> mapM liftBExpr xs
liftBExpr (BFunc f) = return $ Func f
liftBExpr (BLet _ _) = error "liftBExpr: BLet"

-- | The 'resolveExpr' function resolves all names inside an 'Expr' to
-- fully-qualified names.
resolveExpr :: Expr -> Either a Expr
resolveExpr (Const x) = return $ Const x
resolveExpr (App f xs) = App f <$> mapM resolveExpr xs
resolveExpr (Func f) = return $ Func f
