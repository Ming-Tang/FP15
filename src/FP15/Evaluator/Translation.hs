{-# LANGUAGE FlexibleInstances, DeriveGeneric #-}
{-# LANGUAGE BangPatterns, ViewPatterns #-}
module FP15.Evaluator.Translation where
import GHC.Generics
import Prelude hiding (lookup)
import Control.Monad
import Control.DeepSeq
import Data.Maybe(fromMaybe)
import Data.Map(Map, lookup)
import qualified Data.Map as M
import FP15.Value(Value(..))
import FP15.Evaluator.Types
import FP15.Evaluator.Error
import FP15.Evaluator.Standard
import FP15.Evaluator.Contract

type Ident = String
data BaseExpr = Const Value
              | Func Ident
              | Compose [BaseExpr]
              | If BaseExpr BaseExpr BaseExpr
              | Fork [BaseExpr]
              | Pass [BaseExpr]
              | Map BaseExpr
              | Filter BaseExpr
              deriving (Eq, Show, Read, Generic)

instance NFData BaseExpr

-- | The 'transMap' function translates a map of identifiers by name to a map of
-- functions. Notice the map is self-referential therefore 'Data.Map.Strict.Map'
-- would not work.
transMap :: Map Ident Func -> Map Ident BaseExpr -> Map Ident Func
-- | The 'trans' function assembles a function from a 'BaseExpr' and an
-- identifier resolver that takes an identifier and returns a function for that
-- identifier.
trans :: (Ident -> Func) -> BaseExpr -> Func

transMap m0 m = m'
  where
    m' = M.map (trans lookupFunc) m
    lookupFunc n = fromMaybe (getPrim n) (lookup n m')
    getPrim s =
      fromMaybe (const $ raiseErrorMessage $ "Function not found: " ++ show s)
                (lookup s m0)

trans e (Const v) = \(force -> _) -> return v
trans e (Func f) = markFunc f . e f

trans e (Compose fs) = compose $ map (trans e) fs
  where compose = foldl c2 return
        a = map (trans e) fs
        c2 fa fb (force -> x) = fa x >>= fb

trans e (If p f g) = markFunc "If" . \(force -> x) ->
  do b <- predOnly p' x
     if b then f' x else g' x
     where (p', f', g') = (trans e p, trans e f, trans e g)

trans e (Fork fs) = evalFork $ map (trans e) fs
trans e (Pass fs) = evalPass $ map (trans e) fs

trans e (Map f) = evalMap $ trans e f
trans e (Filter p) = evalFilter $ trans e p

-- Evaluation helper functions

listApply :: ([Value] -> ResultOf [Value]) -> Value -> ResultOf Value
listApply f (force -> x) = liftM List $ f =<< (ensure listAnyC x)

evalFork :: [Func] -> Func
evalPass :: [Func] -> Func
evalMap :: Func -> Func
evalFilter :: Func -> Func

evalFork fs (force -> x) = markFunc "Fork" $ liftM List $ mapM ($ x) fs
evalPass fs = markFunc "Pass" . listApply pass
  where pass xs = if length xs == length fs
                  then zipWithM ($) fs $!! xs
                  else raisePassMismatchError (length xs) (length fs)

evalMap f = markFunc "Map" . listApply (mapM f)
evalFilter f = markFunc "Filter" . listApply (filterM $ predOnly f)

