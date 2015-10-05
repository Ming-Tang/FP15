{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module FP15.Evaluator.FPValue where
import GHC.Generics(Generic)
import Control.DeepSeq
import FP15.Disp
import FP15.Value
import FP15.Evaluator.FPRef

-- | The 'FPValue' type represents all possible values in the FP15 runtime. This
-- includes the well-behaved values and the unserializable values such as
-- functions and the @RealWorld@.
type FPValue = XValue Extended
data Extended = Lambda !(FPValue -> FPValue)
              | Ref !(FPRef FPValue)
              | RealWorld !RealWorld
              deriving (Generic)

instance NFData Extended

newtype RealWorld = RW () deriving (Eq, Show, Read, Generic)

instance NFData RealWorld

fromFPValue :: FPValue -> Value
fromFPValue = fmap (\_ -> error "fromFPValue") -- TODO should be a Maybe function...

instance Disp Extended where
  disp (Lambda _) = "#<lambda>#"
  disp (Ref _) = "#<ref>"
  disp (RealWorld _) = "#<RealWorld>"

instance Disp FPValue where
  pretty (Extended x) = pretty x
  pretty v = pretty $ fromFPValue v

class FPValueConvertible t where
  toFPValue :: t -> FPValue
default (FPValue)

instance FPValueConvertible Value where
  toFPValue = fmap (\_ -> error "toFPValue")

instance FPValueConvertible FPValue where
  toFPValue = id

instance FPValueConvertible Bool where
  toFPValue = Bool

instance FPValueConvertible Char where
  toFPValue = Char

instance FPValueConvertible Integer where
  toFPValue = Int

instance FPValueConvertible Int where
  toFPValue = Int . fromIntegral

instance FPValueConvertible Double where
  toFPValue = Real

instance FPValueConvertible String where
  toFPValue = String

instance FPValueConvertible a => FPValueConvertible [a] where
  toFPValue = List . map toFPValue

instance (FPValueConvertible a, FPValueConvertible b)
          => FPValueConvertible (a, b) where
  toFPValue (a, b) = List [toFPValue a, toFPValue b]

instance (FPValueConvertible a, FPValueConvertible b, FPValueConvertible c)
          => FPValueConvertible (a, b, c) where
  toFPValue (a, b, c) = List [toFPValue a, toFPValue b, toFPValue c]

instance (FPValueConvertible a, FPValueConvertible b, FPValueConvertible c,
          FPValueConvertible d)
          => FPValueConvertible (a, b, c, d) where
  toFPValue (a, b, c, d) = List [toFPValue a, toFPValue b, toFPValue c, toFPValue d]

instance (FPValueConvertible a, FPValueConvertible b, FPValueConvertible c,
          FPValueConvertible d, FPValueConvertible e)
          => FPValueConvertible (a, b, c, d, e) where
  toFPValue (a, b, c, d, e) = List [toFPValue a, toFPValue b, toFPValue c, toFPValue d
                                 , toFPValue e]
