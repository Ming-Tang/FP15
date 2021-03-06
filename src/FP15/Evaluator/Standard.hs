{-# LANGUAGE RankNTypes, ExistentialQuantification, ImpredicativeTypes #-}
module FP15.Evaluator.Standard where
import FP15.Disp
import Control.Applicative hiding (Const)
import Data.List(transpose, sort)
import Prelude hiding (div)
import Control.Monad(liftM, (>=>))
import qualified Data.Map.Strict as M
import Data.Maybe(isJust)
import FP15.Value
import FP15.Evaluator.FP (runIO)
import FP15.Evaluator.Types
import FP15.Evaluator.Error
import FP15.Evaluator.Contract
import FP15.Evaluator.Number as N

-- * Function Creation Helpers

-- ** Building Blocks & Contract Enforcement

-- | The 'ensure' function validates a value against a contract, and returns the
-- matched value, or else, raise a 'ContractViolation' error.
ensure :: Contract a -> FPValue -> FP a
-- | The 'ensureFunc' function attaches contracts on the input and output of the
-- 'FPFunc', and the return type will be converted to 'FP' the type the
-- output contract matches.
ensureFunc :: Contract a -> Contract b -> (a -> FPResult) -> FPValue -> FP b
-- | The 'ensureIn' function attaches a contract on the input of the 'FPFunc', and
-- if the contract validates, the matched value will be applied on the function
-- given.
ensureIn :: Contract a -> (a -> FPResult) -> FPValue -> FPResult
-- | The 'ensureOut' function attaches a contract on the output of the 'FPFunc',
-- and if the output validates the output contract, the matched value will be
-- returned.
ensureOut :: Contract b -> (FPValue -> FPResult) -> FPValue -> FP b
-- | The 'predOnly' function attaches a 'BoolC' contract on the output of the
-- 'FPFunc', making sure the function only returns booleans.
predOnly :: FPFunc -> FPValue -> FP Bool

ensure c v =
  case validate c v of
    Nothing -> raiseContractViolation c v
    Just x -> return x

func' :: FPValueConvertible b => Contract a -> (a -> b) -> FPFunc
func :: (ContractConvertible a, FPValueConvertible b) => (a -> b) -> FPFunc
func2 :: (ContractConvertible a, ContractConvertible b,
          FPValueConvertible c) => (a -> b -> c) -> FPFunc

func' c f x = liftM (toFPValue . f) (ensure c x)
func = func' asContract
func2 = func . uncurry

funcE :: (ContractConvertible a, FPValueConvertible b) => (a -> FP b) -> FPFunc
funcE f v = do
  x <- ensure asContract v
  y <- f x
  return $ toFPValue y

ensureFunc a b f x = ensure a x >>= f >>= ensure b
ensureIn = (`ensureFunc` AnyC)
ensureOut = (AnyC `ensureFunc`)
predOnly = ensureOut BoolC

-- ** Math

numListF, numNEListF :: ([Number] -> Number) -> FPFunc
foldN :: (Number -> Number -> Number) -> Number -> FPFunc
foldN1 :: (Number -> Number -> Number) -> FPFunc
subLike :: (b -> a -> b) -> Cons b [a] -> b

numListF f x = liftM (toFPValue . f) (ensure (ListC NumberC) x)
numNEListF f x = (toFPValue . f . consToList) <$> ensure contract x where
  consToList (Cons (x, xs)) = x : xs
  contract = ConsC NumberC (ListC NumberC)
foldN f x0 = numListF (foldl f x0)
foldN1 f = numNEListF (foldl1 f)
subLike f (Cons (a, b)) = foldl f a b

-- ** Equality and Comparison

eq :: [Value] -> Bool
eq (a:b:xs) | a == b = eq (b:xs)
            | otherwise = False
eq _ = True

-- | The 'checkFunc' function creates a 'FPFunc' that returns true if and only if
-- the input value passes the contract provided.
checkFunc :: Contract a -> FPFunc
-- | The 'eqFunc' function creates a 'FPFunc' that returns True if and only if the
-- input values equals the value provided.
eqFunc :: Value -> FPFunc

checkFunc c = return . Bool . isJust . validate c
eqFunc x v = do v' <- ensure ValueC v
                return $ Bool $ (==) v' x

-- ** List Processing

cons :: (FPValue, [FPValue]) -> [FPValue]
decons :: Cons FPValue [FPValue] -> (FPValue, [FPValue])
distl :: (FPValue, [FPValue]) -> [(FPValue, FPValue)]
distr :: ([FPValue], FPValue) -> [(FPValue, FPValue)]
hd :: Cons FPValue [FPValue] -> FPValue
tl :: Cons FPValue [FPValue] -> [FPValue]
zipn :: [[a]] -> [[a]]
index :: ([FPValue], Integer) -> FPResult

cons (a, b) = a : b
decons (Cons (x, xs)) = (x, xs)
distl (a, xs) = map (\x -> (a, x)) xs
distr (xs, a) = map (\x -> (x, a)) xs
hd (Cons (a, b)) = a
tl (Cons (a, b)) = b

zipn xs = if all (not . null) xs
          then map head xs : zipn (map tail xs)
          else []

index (xs, i) = case res of
                  Nothing -> raiseErrorMessage "Index out of range."
                  Just x -> return x
                where res :: Maybe FPValue
                      res = get $ until cond upd (xs, i)
                      cond (l, m) = null l || m == 0
                      upd (_:as, k) = (as, k - 1)
                      upd _ = error "index: upd: impossible"
                      get (a:_, 0) = Just a
                      get ([], _) = Nothing
                      get (a:_, _) = error "index: get: impossible"

-- ** I/O

ioFunc' :: FPValueConvertible r => Contract a -> (a -> IO r) -> FPFunc
ioFunc :: (ContractConvertible a, FPValueConvertible r) => (a -> IO r) -> FPFunc

ioFunc'0 :: ContractConvertible a => (a -> IO r) -> FPFunc
ioFunc'1 :: (ContractConvertible a, FPValueConvertible r) => (a -> IO r) -> FPFunc
ioFunc1'0 :: ContractConvertible a => (a -> IO r) -> FPFunc
ioFunc0'1 ::  FPValueConvertible t => IO t -> FPFunc
ioFunc1'1 :: (ContractConvertible a, FPValueConvertible r) => (a -> IO r) -> FPFunc

ioFunc' c f x = do
  z <- ensure c x
  w <- runIO $ f z
  return $ toFPValue w
  --liftM (runIO . f) (ensure c x)

ioFunc = ioFunc' asContract

ioFunc'0 f = ioFunc (\args -> f args >> return RW)
ioFunc'1 f = ioFunc (f >=> (\y -> return (RW, y)))

ioFunc1'0 f = ioFunc'0 (\(RW, x) -> f x)
ioFunc0'1 f = ioFunc (\RW -> f >>= (\y -> return (RW, y)))
ioFunc1'1 f = ioFunc (\(RW, x) -> f x >>= (\y -> return (RW, y)))

-- cases for IOFunc
--   * () -> IO ()        -- RealWorld^ -> RealWorld^
--   * args -> IO ()      -- RealWorld^ & args -> RealWorld^
--   * () -> IO ret       -- RealWorld^ -> {RealWorld^, arg}
--   * args -> IO ret     -- RealWorld^ & args -> {RealWorld, arg}

-- * Standard Library

-- The 'standardEnv' is the standard environment that includes all standard
-- function definitions with names prefixed with @Std.@
standardEnv :: M.Map String FPFunc
-- 'standardEnv'' is like 'standardEnv' except there is no @Std.@ prefix in
-- front of all names
standardEnv' :: M.Map String FPFunc

standardEnv = M.mapKeys (\n -> "Std." ++ n) standardEnv'
standardEnv' = M.fromList [
    ("_", return)

  -- Math
  , ("min", foldN1 min)
  , ("max", foldN1 max)

  , ("range", func $ either (\(a, b) -> range a b (IntN 1)) (\(a, b, d) -> range a b d))
  , ("xrange", func $ either (\(a, b) -> xrange a b (IntN 1)) (\(a, b, d) -> xrange a b d))

  , ("succ", func (`add` IntN 1))
  , ("pred", func (`sub` IntN 1))
  , ("isEven", func (even :: Integer -> Bool))
  , ("isOdd", func (odd :: Integer -> Bool))

  , ("neg", func neg)
  , ("add", foldN add (IntN 0))
  , ("sub", func $ subLike sub)
  , ("mul", foldN mul (IntN 1))
  , ("div", func $ subLike div)
  , ("mod", func $ subLike N.mod)

  , ("sum", foldN add (IntN 0))
  , ("prod", foldN mul (IntN 1))
  , ("pow", func2 pow)

  , ("sqrt", func N.sqrt)
  , ("sin", func N.sin)
  , ("cos", func N.cos)
  , ("tan", func N.tan)

  , ("sgn", func N.sgn)
  , ("abs", func N.abs)

  , ("gt", func2 greaterThan)
  , ("lt", func2 lessThan)
  , ("ge", func2 greaterEq)
  , ("le", func2 lessEq)
  , ("neq", func2 equals)
  , ("nne", func2 notEquals)

  , ("cons", func cons)
  , ("decons", func decons)
  , ("hd", func hd)
  , ("tl", func tl)

  , ("eq", func eq)
  , ("ne", func (not . eq))

  -- Logic
  , ("and", func (and :: [Bool] -> Bool))
  , ("or", func (or :: [Bool] -> Bool))
  , ("not", func not)

  -- Type Checking
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

  -- List Processing
  , ("s0", func (\x -> head (x :: [FPValue])))
  , ("s1", func (\x -> (x :: [FPValue]) !! 1))
  , ("sort", func (sort :: [Value] -> [Value]))
  , ("distl", func distl)
  , ("distr", func distr)
  , ("rev", func (reverse :: [FPValue] -> [FPValue]))
  , ("reverse", func (reverse :: [FPValue] -> [FPValue]))
  , ("append", func (concat :: [[FPValue]] -> [FPValue]))
  , ("cross", func (sequence :: [[FPValue]] -> [[FPValue]]))
  , ("trans", func (transpose :: [[FPValue]] -> [[FPValue]]))
  , ("zip", func (zipn :: [[FPValue]] -> [[FPValue]]))
  , ("index", funcE index)
  , ("len", func (length :: [FPValue] -> Int))

  -- I/O
  , ("put", ioFunc1'0 (\x -> putStr $ disp (x :: FPValue)))
  , ("putL", ioFunc1'0 (\x -> putStrLn $ disp (x :: FPValue)))
  , ("putS", ioFunc1'0 (\(Str s) -> putStr s))
  , ("putSL", ioFunc1'0 (\(Str s) -> putStrLn s))
  , ("putC", ioFunc1'0 putChar)

  , ("getL", ioFunc0'1 (Str <$> getLine))
  , ("getC", ioFunc0'1 getChar)
  ] :: M.Map String FPFunc
