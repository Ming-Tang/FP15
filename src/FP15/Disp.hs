{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module FP15.Disp where
import Text.PrettyPrint
import qualified Data.Map as M
import Data.Void(Void)

-- * The Disp Typeclass

-- | The 'Disp' typeclass is for converting a value to string. The difference
-- betwen 'Disp' and 'Show' is that 'Show' is made to be parseable by 'Read' and
-- 'Disp' is for user-friendly representation. Error types use 'Disp' to
-- generate user-readable error messages.
class Disp a where
  {-# MINIMAL disp | pretty #-}
  disp :: a -> String
  pretty :: a -> Doc

  disp = show . pretty
  pretty = text . disp

-- TODO more ways to configure disp: depth control, show location, etc.

instance Disp Void where
  disp = undefined

instance Disp () where
  disp = show

instance Disp String where
  disp = id
  pretty = text

instance (Disp k, Disp v) => Disp (M.Map k v) where
  pretty = vcat . map (\(k, v) -> pretty k <> colon <+> nest 2 (pretty v)) . M.toList

