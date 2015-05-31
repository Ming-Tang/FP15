{-# LANGUAGE FlexibleInstances #-}
module FP15.Value where

data Value = Bool Bool
           | Char Char
           | Int Integer
           | Real Float
           | Symbol String
           | String String
           | List [Value]
            deriving (Eq, Show, Read)

class Eq t => ValueConvertible t where
  toValue :: t -> Value

instance ValueConvertible Value where
  toValue = id

instance ValueConvertible Bool where
  toValue = Bool

instance ValueConvertible Char where
  toValue = Char

instance ValueConvertible Integer where
  toValue = Int

instance ValueConvertible Float where
  toValue = Real

instance ValueConvertible String where
  toValue = String

instance ValueConvertible [Value] where
  toValue = List

