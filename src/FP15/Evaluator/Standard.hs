{-# LANGUAGE RankNTypes, ExistentialQuantification, ImpredicativeTypes #-}
module FP15.Evaluator.Standard where
import Data.List(transpose)
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

funcE :: (ContractConvertible a, ValueConvertible b) => (a -> ResultOf b) -> Func
funcE f v = do
  x <- ensure asContract v
  y <- f x
  return $ toValue y

ensureFunc a b f x = ensure a x >>= f >>= ensure b
ensureIn = (`ensureFunc` AnyC)
ensureOut = (AnyC `ensureFunc`)
predOnly = ensureOut BoolC

numListF :: ([Number] -> Number) -> Func
foldN :: (Number -> Number -> Number) -> Number -> Func
numListF f x = liftM (toValue . f) (ensure (ListC NumberC) x)
foldN f x0 = numListF (foldl f x0)

eq :: [Value] -> Bool
eq (a:b:xs) | a == b = eq (b:xs)
            | otherwise = False
eq _ = True

cons :: (Value, [Value]) -> [Value]
decons :: Cons Value [Value] -> (Value, [Value])
distl :: (Value, [Value]) -> [(Value, Value)]
distr :: ([Value], Value) -> [(Value, Value)]

cons (a, b) = a : b
decons (Cons (x, xs)) = (x, xs)
distl (a, xs) = map (\x -> (a, x)) xs
distr (xs, a) = map (\x -> (x, a)) xs

subLike :: (b -> a -> b) -> Cons b [a] -> b
subLike f (Cons (a, b)) = foldl f a b

-- | The 'checkFunc' function creates a 'Func' that returns true if and only if
-- the input value passes the contract provided.
checkFunc :: Contract a -> Func
-- | The 'eqFunc' function creates a 'Func' that returns True if and only if the
-- input values equals the value provided.
eqFunc :: Value -> Func

checkFunc c = return . Bool . isJust . validate c
eqFunc x = return . Bool . (==) x

index :: ([Value], Integer) -> ResultOf Value
index (xs, i) = case res of
                  Nothing -> raiseErrorMessage "Index out of range."
                  Just x -> return x
                where res :: Maybe Value
                      res = get $ until cond upd (xs, i)
                      cond (l, m) = l == [] || m == 0
                      upd (_:as, k) = (as, k - 1)
                      upd _ = undefined
                      get (a:_, 0) = Just a
                      get ([], _) = Nothing
                      get (a:_, _) = undefined

standardEnv', standardEnv :: M.Map String Func

standardEnv = M.mapKeys (\n -> "Std." ++ n) standardEnv'
standardEnv' = M.fromList [
    ("_", return)

  , ("range", func2 (enumFromTo :: Integer -> Integer -> [Integer]))
  , ("xrange", func2 ((\x y -> [x..y - 1]) :: Integer -> Integer -> [Integer]))
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

  , ("gt", func2 ((>) :: Value -> Value -> Bool))
  , ("lt", func2 ((<) :: Value -> Value -> Bool))
  , ("ge", func2 ((>=) :: Value -> Value -> Bool))
  , ("le", func2 ((<=) :: Value -> Value -> Bool))

  , ("f", func $ (const False :: Value -> Bool))
  , ("t", func $ (const True :: Value -> Bool))

  , ("cons", func cons)
  , ("decons", func decons)

  , ("eq", func eq)
  , ("ne", func (not . eq))

  , ("and", func and)
  , ("or", func or)
  , ("not", func $ not)

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

  , ("distl", func distl)
  , ("distr", func distl)
  , ("reverse", func (reverse :: [Value] -> [Value]))
  , ("append", func (concat :: [[Value]] -> [Value]))
  , ("cross", func (sequence :: [[Value]] -> [[Value]]))
  , ("trans", func (transpose :: [[Value]] -> [[Value]]))
  , ("index", funcE index)
  , ("len", func (length :: [Value] -> Int))
  ] :: M.Map String Func
