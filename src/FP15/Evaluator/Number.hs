module FP15.Evaluator.Number where
import Prelude hiding (div)
import Data.Char(ord)
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

numericBinOp' :: (Integer -> Integer -> a)
                 -> (Float -> Float -> a)
                 -> Number -> Number -> a

numericBinOp :: (Integer -> Integer -> Integer)
                -> (Float -> Float -> Float)
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


numericUnOp f g = numericUnOp' (IntN . f) (RealN . g)
numericBinOp f g = numericBinOp' (ub IntN f) (ub RealN g) where ub = (.) (.) (.)

-- * Operations

isZero :: Number -> Bool
isZero (CharN '\0') = True
isZero (IntN 0) = True
isZero (RealN 0) = True
isZero _ = False

add = numericBinOp (+) (+)
sub = numericBinOp (-) (-)
mul = numericBinOp (*) (*)
div = numericBinOp quot (/)
sgn = numericUnOp signum signum
absolute = numericUnOp abs abs

greaterThan = numericBinOp' (>) (>)
lessThan = numericBinOp' (<) (<)
greaterEq = numericBinOp' (>=) (>=)
lessEq = numericBinOp' (<=) (<=)

