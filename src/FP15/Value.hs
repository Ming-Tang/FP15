{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
module FP15.Value where
import GHC.Generics
import Control.DeepSeq
import Control.Applicative
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
instance NFData Value where rnf x = seq x ()

convXValue f = conv where
  conv (Bool b) = Bool b
  conv (Char c) = Char c
  conv (Int i) = Int i
  conv (Real r) = Real r
  conv (Symbol s) = Symbol s
  conv (String s) = String s
  conv (List xs) = List $ map conv xs
  conv (Extended x) = Extended x

convXValueM f = conv where
  conv (Bool b) = pure $ Bool b
  conv (Char c) = pure $ Char c
  conv (Int i) = pure $ Int i
  conv (Real r) = pure $ Real r
  conv (Symbol s) = pure $ Symbol s
  conv (String s) = pure $ String s
  conv (List xs) = List <$> mapM conv xs
  conv (Extended x) = f x

convToValue :: XValue t -> Maybe (XValue Void)
convToValue = convXValueM (const Nothing)

-- | The 'prettyXValue' function pretty prints an 'XValue' like 'Value's except
-- the 'Extended' case is pretty printed with a custom function.
prettyXValue :: (t -> Doc) -> XValue t -> Doc
prettyXValue f = pretty where
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
  pretty (Extended x) = f x

instance Disp Value where
  -- To prevent a compiler warning.
  pretty = prettyXValue (\_ -> error "instance Disp Value: pretty: impossible")

  disp = show . pretty
