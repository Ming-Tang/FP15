{-# LANGUAGE Safe, FlexibleInstances, DeriveGeneric #-}
module FP15.Value where
import GHC.Generics
import Control.DeepSeq

data Value = Bool Bool
           | Char Char
           | Int Integer
           | Real Double
           | Symbol String
           | String String
           | List [Value]
            deriving (Eq, Ord, Show, Read, Generic)

instance NFData Value

class Eq t => ValueConvertible t where
  toValue :: t -> Value
default (Value)

instance ValueConvertible Value where
  toValue = id

instance ValueConvertible Bool where
  toValue = Bool

instance ValueConvertible Char where
  toValue = Char

instance ValueConvertible Integer where
  toValue = Int

instance ValueConvertible Int where
  toValue = Int . fromIntegral

instance ValueConvertible Double where
  toValue = Real

instance ValueConvertible String where
  toValue = String

instance ValueConvertible a => ValueConvertible [a] where
  toValue = List . map toValue

instance (ValueConvertible a, ValueConvertible b)
          => ValueConvertible (a, b) where
  toValue (a, b) = List [toValue a, toValue b]

instance (ValueConvertible a, ValueConvertible b, ValueConvertible c)
          => ValueConvertible (a, b, c) where
  toValue (a, b, c) = List [toValue a, toValue b, toValue c]

instance (ValueConvertible a, ValueConvertible b, ValueConvertible c,
          ValueConvertible d)
          => ValueConvertible (a, b, c, d) where
  toValue (a, b, c, d) = List [toValue a, toValue b, toValue c, toValue d]

instance (ValueConvertible a, ValueConvertible b, ValueConvertible c,
          ValueConvertible d, ValueConvertible e)
          => ValueConvertible (a, b, c, d, e) where
  toValue (a, b, c, d, e) = List [toValue a, toValue b, toValue c, toValue d
                                 , toValue e]
