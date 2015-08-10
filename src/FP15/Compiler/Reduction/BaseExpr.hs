{-# LANGUAGE ViewPatterns #-}
module FP15.Compiler.Reduction.BaseExpr (
  BaseExprError(..)
, toBaseExpr
) where
import Control.Applicative hiding (Const)
import FP15.Standard(stdName)
import FP15.Evaluator.Types as E
import FP15.Types hiding (Expr(..))
import qualified FP15.Types as T

data BaseExprError = InvalidFl (LocName Fl)
                   | InvalidFlArity (LocName Fl) Int
                   | BaseF
                   deriving (Eq, Ord, Show, Read)

toBaseExpr :: T.Expr -> Either BaseExprError BaseExpr
toBaseExpr (T.Const c) = return $ E.Const c
toBaseExpr (T.Func (T.Loc _ f)) = return $ E.Func $ disp f

toBaseExpr (ta -> Just ("BaseF", [f])) = Left BaseF
toBaseExpr (ta -> Just ("If", [p, a])) = If <$> p <*> a <*> pure (E.Func "Std._")
toBaseExpr (ta -> Just ("If", [p, a, b])) = If <$> p <*> a <*> b
toBaseExpr (ta -> Just ("Compose", xs)) = Compose <$> sequence xs
toBaseExpr (ta -> Just ("Fork", xs)) = Fork <$> sequence xs
toBaseExpr (ta -> Just ("Hook", xs)) = Hook <$> sequence xs
toBaseExpr (ta -> Just ("While", [p, f])) = While <$> p <*> f
toBaseExpr (ta -> Just ("Filter", [f])) = Filter <$> f
toBaseExpr (ta -> Just ("Map", [f])) = Map <$> f
toBaseExpr (T.App f@(Loc _ nf) (length -> n)) =
  if nf `elem` map stdName ["BaseF", "If", "Compose", "Fork", "Hook", "While", "Filter", "Map"] then
    Left (InvalidFlArity f n)
  else
    Left (InvalidFl f)

ta :: T.Expr -> Maybe (String, [Either BaseExprError BaseExpr])
ta (T.App (Loc _ (N ["Std"] n)) (map toBaseExpr -> xs)) = Just (n, xs)
ta _ = undefined

