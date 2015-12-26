module FP15.Evaluator.Number (module FP15.Evaluator.Number, Number(..)) where
import Prelude hiding (div)
import Prelude as P
import Data.Char(ord)
import Data.Fixed(mod')
import FP15.Evaluator.Types

-- | 'zeroN' is number for zero as integer. Note that to check if a number is
-- zero, 'isZero' should be used instead, because there can be zeroes in other
-- numeric types.
zeroN :: Number
zeroN = IntN 0

-- * Type Conversion

-- | The 'getTower' function determines the position of a value in the numerical
-- tower
getTower :: Number -> NumTower
-- | The 'raiseTower' function coerces a number into a higher position in the
-- numerical tower
raiseTower :: Number -> NumTower -> Number
-- | The 'coercePair' function coerces two numbers to the one higher up in the
-- numerical tower
coercePair :: Number -> Number -> (Number, Number)

fromChar :: Char -> Integer
fromChar = fromIntegral . ord

getTower (CharN _) = CharT
getTower (IntN _) = IntT
getTower (RealN _) = RealT

raiseTower a@(CharN _) CharT = a
raiseTower (CharN c) IntT = IntN $ fromChar c
raiseTower (CharN c) RealT = RealN $ fromIntegral $ fromChar c

raiseTower a@(IntN _) CharT = a
raiseTower a@(IntN _) IntT = a
raiseTower (IntN i) RealT = RealN $ fromIntegral i

raiseTower a@(RealN _) CharT = a
raiseTower a@(RealN _) IntT = a
raiseTower a@(RealN _) RealT = a

-- * Operator Creation
numericUnOp' :: (Integer -> a) -> (Double -> a) -> Number -> a

numericBinOp' :: (Integer -> Integer -> a)
                 -> (Double -> Double -> a)
                 -> Number -> Number -> a

numericBinOp :: (Integer -> Integer -> Integer)
                -> (Double -> Double -> Double)
                -> Number -> Number -> Number

coercePair a b =
  let t = max (getTower a) (getTower b) in
  (raiseTower a t, raiseTower b t)

numericUnOp' f g a =
  case a of
    CharN c -> f (fromChar c)
    IntN i -> f i
    RealN r -> g r

numericBinOp' f g a b = let numPairC = Args2C NumberC NumberC in
  case coercePair a b of
    (CharN i, CharN j) -> f (fromChar i) (fromChar j)
    (IntN i, IntN j) -> f i j
    (RealN x, RealN y) -> g x y
    -- I can avoid this impossible case using GADTs like Contract did.
    -- I tried an I failed.
    _ -> error "impossible: coercePair did something wrong"


numericUnOp :: (Integer -> Integer) -> (Double -> Double) -> Number -> Number
numericRealUnOp :: (Double -> Double) -> Number -> Number
numericBinRealOp :: (Double -> Double -> Double) -> Number -> Number -> Number

numericUnOp f g = numericUnOp' (IntN . f) (RealN . g)
numericBinOp f g = numericBinOp' (ub IntN f) (ub RealN g) where ub = (.) (.) (.)
numericRealUnOp f = numericUnOp' (RealN . f . fromIntegral) (RealN . f)
numericBinRealOp f = numericBinOp' (\x y -> RealN $ f (fromIntegral x) (fromIntegral y))
                                   (\x y -> RealN $ f x y)

-- * Operations

isZero :: Number -> Bool
isZero (CharN '\0') = True
isZero (IntN 0) = True
isZero (RealN 0) = True
isZero _ = False

add, sub, mul, div :: Number -> Number -> Number
sgn, abs, neg :: Number -> Number

add = numericBinOp (+) (+)
sub = numericBinOp (-) (-)
mul = numericBinOp (*) (*)
div = numericBinOp quot (/)
mod = numericBinOp P.mod mod'
sgn = numericUnOp signum signum
abs = numericUnOp P.abs P.abs
neg = numericUnOp P.negate P.negate

greaterThan, lessThan, greaterEq, lessEq, equals, notEquals :: Number -> Number -> Bool
greaterThan = numericBinOp' (>) (>)
lessThan = numericBinOp' (<) (<)
greaterEq = numericBinOp' (>=) (>=)
lessEq = numericBinOp' (<=) (<=)
equals = numericBinOp' eq' eq' where eq' x y = x <= y && x >= y
notEquals = numericBinOp' ne' ne' where ne' x y = not(x <= y && x >= y)

pow :: Number -> Number -> Number
pow = numericBinOp (^) (**)

sqrt, sin, cos, tan :: Number -> Number
sqrt = numericRealUnOp P.sqrt
sin = numericRealUnOp P.sin
cos = numericRealUnOp P.cos
tan = numericRealUnOp P.tan

rng :: (Number -> Number -> Bool) -> Number -> Number -> Number -> [Number]
rng p a b d = reverse $ snd $ until (\(x, _) -> not (p x b)) (\(x, ys) -> (add x d, x:ys)) (a, [])

range, xrange :: Number -> Number -> Number -> [Number]
range = rng lessEq
xrange = rng lessThan

-- TODO dedicated module for number type
-- TODO factor in Number to Value
instance Num Number where
  abs = FP15.Evaluator.Number.abs
  negate = FP15.Evaluator.Number.neg
  signum = FP15.Evaluator.Number.sgn
  (+) = add
  (*) = mul
  (-) = sub
  fromInteger = IntN

instance Ord Number where
  (<=) = lessEq

