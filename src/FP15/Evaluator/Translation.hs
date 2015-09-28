{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module FP15.Evaluator.Translation where
import Prelude hiding (lookup)
import Control.Monad
import Control.Monad.Error
import Control.DeepSeq
import Data.Maybe(fromMaybe)
import Data.Map(Map, lookup)
import qualified Data.Map as M
import FP15.Name
import FP15.Value
import FP15.Evaluator.Types
import FP15.Evaluator.Error
import FP15.Evaluator.Standard
import FP15.Evaluator.Contract
import FP15.Evaluator.FP
import FP15.Evaluator.FPEnv

conv :: Either RuntimeError a -> FP a
conv = either throwError (return . id)

predOnlyX :: FPFunc -> FPValue -> FP Bool
predOnlyX p = p >=> ensure BoolC

-- | The 'transMap' function translates a map of identifiers by name to a map of
-- functions. Notice the map is self-referential therefore 'Data.Map.Strict.Map'
-- would not work.
transMap :: Map Ident FPFunc -> Map Ident BaseExpr -> Map Ident FPFunc
-- | The 'trans' function assembles a function from a 'BaseExpr' and an
-- identifier resolver that takes an identifier and returns a function for that
-- identifier.
trans :: (Ident -> FPFunc) -> BaseExpr -> FPFunc

transMap m0 m = m'
  where
    m' = M.map (trans lookupFunc) m
    lookupFunc n = fromMaybe (getPrim n) (lookup n m')
    getPrim s =
      fromMaybe (const $ raiseErrorMessage $ "Function not found: " ++ show s)
                (lookup s m0)

trans e (Const v) = \(force -> _) -> return $ toFPValue v
trans e (Func lf@(Loc _ f)) = markFunc lf . e f

trans e (Compose fs) = compose $ map (trans e) fs
  where compose = foldl c2 return
        a = map (trans e) fs
        c2 fa fb (force -> x) = fa x >>= fb

trans e (If p f g) = \(force -> x) ->
  do b <- predOnlyX p' x
     if b then f' x else g' x
     where (p', f', g') = (trans e p, trans e f, trans e g)

trans e (Fork fs) = evalFork $ map (trans e) fs
trans e (Hook fs) = evalHook $ map (trans e) fs

trans e (Map f) = evalMap $ trans e f
trans e (Filter p) = evalFilter $ trans e p

trans e (While p f) = body where
  body (force -> !x) = do
    b <- predOnlyX p' x
    if b then f' x >>= body
    else return x
  (p', f') = (trans e p, trans e f)

trans e (Get i) = body where
  body (force -> !x) = do
    env <- getEnv
    case get i env of
      Nothing -> error "Access violation"
      Just v -> return v

trans e (With e1) = body where
  inner = force $ trans e e1
  body (force -> !x) = do
    withEnv (push x) $ inner x

trans e (Mark k x) = markFunc (noLoc k) . trans e x

-- Evaluation helper functions

listApply :: ([FPValue] -> FP [FPValue]) -> FPValue -> FP FPValue
listApply f (force -> x)
  = ensure listAnyC x >>= f . map toFPValue >>= return . toFPValue . List

evalFork :: [FPFunc] -> FPFunc
evalHook :: [FPFunc] -> FPFunc
evalMap :: FPFunc -> FPFunc
evalFilter :: FPFunc -> FPFunc

evalFork fs (force -> x) = liftM List $ mapM ($ x) fs
evalHook fs = listApply hook
  where hook xs = if length xs == length fs
                  then zipWithM ($) fs $!! xs
                  else raisePassMismatchError (length xs) (length fs)

evalMap f = listApply (mapM f)
evalFilter f = listApply (filterM $ predOnlyX f)

