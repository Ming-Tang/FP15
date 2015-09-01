{-# LANGUAGE Safe, StandaloneDeriving, FlexibleInstances, DeriveGeneric, DeriveFunctor, EmptyDataDecls #-}
module FP15.Value where
import GHC.Generics
import Control.DeepSeq
import FP15.Disp
import Text.PrettyPrint

data Void
deriving instance Eq Void
deriving instance Ord Void
deriving instance Show Void
deriving instance Read Void
deriving instance Generic Void

data XValue x = Bool Bool
              | Char Char
              | Int Integer
              | Real Double
              | Symbol String
              | String String
              | List [XValue x]
              | Extended !x
              deriving (Eq, Ord, Show, Read, Generic, Functor)

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

  pretty (Extended _) = error "instance Disp Value: impossible"

  disp = show . pretty
