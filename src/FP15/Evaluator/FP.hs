{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module FP15.Evaluator.FP where
import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans
import FP15.Types
import FP15.Value
import FP15.Evaluator.FPRef
import FP15.Evaluator.XValue

newtype FP a = FP { runFP :: IO a }
deriving instance MonadIO FP
deriving instance MonadFix FP

instance Monad FP where
  return = FP . return
  (FP a) >>= b = FP (a >>= fmap runFP b)

instance Applicative FP where
  pure = return
  (<*>) = ap

instance Functor FP where
  fmap f (FP x) = FP $ fmap f x
