{-# LANGUAGE RankNTypes, ExistentialQuantification, ImpredicativeTypes #-}
module FP15.Evaluator.Standard where
import Prelude hiding (div)
import Control.Monad(liftM)
import qualified Data.Map.Strict as M
import Data.Maybe(isJust)
import FP15.Value
import FP15.Evaluator.Types
import FP15.Evaluator.Error
import FP15.Evaluator.Contract
import FP15.Evaluator.Number

ensure :: Contract a -> Value -> ResultOf a
ensure c v =
  case validate c v of
    Nothing -> raiseContractViolation c v
    Just x -> return x

-- | Attach contracts on the input and output of a function.
ensureFunc :: Contract a -> Contract b -> (a -> Result) -> Value -> ResultOf b
ensureL :: Contract a -> (a -> Result) -> Value -> ResultOf Value
ensureR :: Contract b -> (Value -> Result) -> Value -> ResultOf b
predOnly :: Func -> Value -> ResultOf Bool

func' :: ValueConvertible b => Contract a -> (a -> b) -> Func
func :: (ContractConvertible a, ValueConvertible b) => (a -> b) -> Func
func' c f x = liftM (toValue . f) (ensure c x)
func = func' asContract

ensureFunc a b f x = ensure a x >>= f >>= ensure b
ensureL = (`ensureFunc` AnyC)
ensureR = (AnyC `ensureFunc`)
predOnly = ensureR BoolC

numListF :: ([Number] -> Number) -> Func
foldN :: (Number -> Number -> Number) -> Number -> Func
numListF f x = liftM (toValue . f) (ensure (ListC NumberC) x)
foldN f x0 = numListF (foldl f x0)

cons :: (Value, [Value]) -> [Value]
cons (a, b) = a : b
uncons (Cons (x, xs)) = [x, List xs]
distl (a, xs) = map (\x -> List [a, x]) xs
distr (xs, a) = map (\x -> List [x, a]) xs

subLike f (Cons (a, b)) = foldl f a b

checkFunc :: Contract a -> Func
eqFunc :: Value -> Func

checkFunc c = return . Bool . isJust . validate c

eqFunc x = return . Bool . (==) x

standardEnv =
  M.fromList [
    ("i", return)

  , ("succ", func (`add` IntN 1))
  , ("pred", func (`sub` IntN 1))
  , ("even?", func (even :: Integer -> Bool))
  , ("odd?", func (odd :: Integer -> Bool))

  , ("+", foldN add (IntN 0))
  , ("-", func $ subLike sub)
  , ("*", foldN mul (IntN 1))
  , ("-", func $ subLike div)
  , ("sgn", func sgn)
  , ("abs", func absolute)

  , ("distl", func distl)
  , ("distr", func distl)
  , ("cons", func cons)
  , ("uncons", func uncons)

  , ("zero?", func isZero)
  , ("true?", eqFunc $ Bool True)
  , ("false?", eqFunc $ Bool False)

  , ("bool?", checkFunc BoolC)

  , ("char?", checkFunc CharC)
  , ("int?", checkFunc IntC)
  , ("real?", checkFunc RealC)
  , ("number?", checkFunc NumberC)

  , ("symbol?", checkFunc SymbolC)
  , ("string?", checkFunc StringC)

  , ("list?", checkFunc listAnyC)
  , ("empty?", checkFunc EmptyC)
  , ("cons?", checkFunc $ ConsC AnyC listAnyC)
  ] :: M.Map String Func
