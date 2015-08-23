{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module FP15.Disp where
import Text.PrettyPrint

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

instance Disp String where
  disp = id
  pretty = text
