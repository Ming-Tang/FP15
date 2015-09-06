{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module FP15.Evaluator.FP where
import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Error
import FP15.Evaluator.RuntimeError

-- | The 'FP' monad is for executing FP15 code. This monad contains 'IO' and
-- error handling ('RuntimeError').
newtype FP a = FP { runFP :: ErrorT RuntimeError IO a }

deriving instance MonadIO FP
deriving instance MonadFix FP
deriving instance MonadError RuntimeError FP

instance Monad FP where
  return = FP . return
  (FP a) >>= b = FP (a >>= fmap runFP b)

instance Applicative FP where
  pure = return
  (<*>) = ap

instance Functor FP where
  fmap f (FP x) = FP $ fmap f x
