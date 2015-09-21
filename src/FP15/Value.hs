{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
module FP15.Value where
import GHC.Generics
import Control.DeepSeq
import FP15.Xtn
import FP15.Disp
import Text.PrettyPrint
import Data.Void(Void)

-- | The 'XValue' type is the base type for all FP15 values with extra cases of type @x@.
-- The extra cases are for the FP15 runtime.
data XValue x
  = Bool Bool
  | Char Char
  | Int Integer
  | Real Double
  | Symbol String
  | String String
  | List [XValue x]
  | Extended !x  -- ^ Strict to make this case impossible for `XValue Void`.
  deriving (Eq, Ord, Show, Read, Generic, Functor)

instance Xtn XValue where
  maybeX (Extended x) = Just x
  maybeX _ = Nothing
  fromX = Extended

-- TODO Map type

-- | A `Value' represents an FP15 value that is well-behaved: Has structural
-- equality and comparison, is serializable, and can be literally expressed in
-- FP15 code.
type Value = XValue Void

instance NFData x => NFData (XValue x)
instance NFData Value

instance Disp Value where
  pretty (Bool False) = text "#f"
  pretty (Bool True) = text "#t"
  pretty (Char c) = text ("#\\" ++ [c])
  pretty (Int i) | i < 0 = text ("~" ++ show (negate i))
               | otherwise = text (show i)
  pretty (Real r) | r < 0 = text ("~" ++ show (negate r))
                | otherwise = text (show r) -- won't work on some edge cases
  pretty (Symbol s) = text ("'" ++ s)
  pretty (String s) = text (show s) -- doesn't handle escape well
  pretty (List xs) =
   lbrack <> fsep (punctuate comma $ map (nest 2 . pretty) xs) <> rbrack

  -- To prevent a compiler warning.
  pretty (Extended _) = error "instance Disp Value: impossible"

  disp = show . pretty
