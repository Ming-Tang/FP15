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
import FP15.Evaluator.Number as N

-- | The 'ensure' function validates a value against a contract, and returns the
-- matched value, or else, raise a 'ContractViolation' error.
ensure :: Contract a -> Value -> ResultOf a
-- | The 'ensureFunc' function attaches contracts on the input and output of the
-- 'Func', and the return type will be converted to 'ResultOf' the type the
-- output contract matches.
ensureFunc :: Contract a -> Contract b -> (a -> Result) -> Value -> ResultOf b
-- | The 'ensureIn' function attaches a contract on the input of the 'Func', and
-- if the contract validates, the matched value will be applied on the function
-- given.
ensureIn :: Contract a -> (a -> Result) -> Value -> Result
-- | The 'ensureOut' function attaches a contract on the output of the 'Func',
-- and if the output validates the output contract, the matched value will be
-- returned.
ensureOut :: Contract b -> (Value -> Result) -> Value -> ResultOf b
-- | The 'predOnly' function attaches a 'BoolC' contract on the output of the
-- 'Func', making sure the function only returns booleans.
predOnly :: Func -> Value -> ResultOf Bool

ensure c v =
  case validate c v of
    Nothing -> raiseContractViolation c v
    Just x -> return x

func' :: ValueConvertible b => Contract a -> (a -> b) -> Func
func :: (ContractConvertible a, ValueConvertible b) => (a -> b) -> Func
func2 :: (ContractConvertible a, ContractConvertible b,
          ValueConvertible c) => (a -> b -> c) -> Func

func' c f x = liftM (toValue . f) (ensure c x)
func = func' asContract
func2 = func . uncurry

ensureFunc a b f x = ensure a x >>= f >>= ensure b
ensureIn = (`ensureFunc` AnyC)
ensureOut = (AnyC `ensureFunc`)
predOnly = ensureOut BoolC

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

-- | The 'checkFunc' function creates a 'Func' that returns true if and only if
-- the input value passes the contract provided.
checkFunc :: Contract a -> Func
-- | The 'eqFunc' function creates a 'Func' that returns True if and only if the
-- input values equals the value provided.
eqFunc :: Value -> Func

checkFunc c = return . Bool . isJust . validate c
eqFunc x = return . Bool . (==) x

standardEnv', standardEnv :: M.Map String Func

standardEnv = M.mapKeys (\n -> "Std." ++ n) standardEnv'
standardEnv' = M.fromList [
    ("_", return)

  , ("succ", func (`add` IntN 1))
  , ("pred", func (`sub` IntN 1))
  , ("isEven", func (even :: Integer -> Bool))
  , ("isOdd", func (odd :: Integer -> Bool))

  , ("neg", func neg)
  , ("add", foldN add (IntN 0))
  , ("sub", func $ subLike sub)
  , ("mul", foldN mul (IntN 1))
  , ("div", func $ subLike div)

  , ("sum", foldN add (IntN 0))
  , ("prod", foldN mul (IntN 1))
  , ("pow", func2 pow)

  , ("sqrt", func N.sqrt)
  , ("sin", func N.sin)
  , ("cos", func N.cos)
  , ("tan", func N.tan)

  , ("sgn", func N.sgn)
  , ("abs", func N.abs)

  , ("gt", func2 N.greaterThan)
  , ("lt", func2 N.lessThan)
  , ("ge", func2 N.greaterEq)
  , ("le", func2 N.lessEq)

  , ("distl", func distl)
  , ("distr", func distl)
  , ("cons", func cons)
  , ("uncons", func uncons)

  -- TODO make comp functions variadic
  , ("eq", func2 ((==) :: Value -> Value -> Bool))
  , ("ne", func2 ((/=) :: Value -> Value -> Bool))

  , ("is0", func isZero)
  , ("isT", eqFunc $ Bool True)
  , ("isF", eqFunc $ Bool False)

  , ("isBool", checkFunc BoolC)

  , ("isChar", checkFunc CharC)
  , ("isInt", checkFunc IntC)
  , ("isReal", checkFunc RealC)
  , ("isNum", checkFunc NumberC)

  , ("isSymbol", checkFunc SymbolC)
  , ("isString", checkFunc StringC)

  , ("isList", checkFunc listAnyC)
  , ("isEmpty", checkFunc EmptyC)
  , ("isCons", checkFunc $ ConsC AnyC listAnyC)

  , ("len", func (length :: [Value] -> Int))
  ] :: M.Map String Func
