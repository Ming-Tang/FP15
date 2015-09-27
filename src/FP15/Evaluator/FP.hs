{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module FP15.Evaluator.FP where
import Control.Applicative
import Control.Monad.RWS
import Control.Monad.Error
import FP15.Evaluator.RuntimeError

-- | The 'FP' monad is for executing FP15 code. This monad contains 'IO' and
-- error handling ('RuntimeError').
newtype FP a = FP { unFP :: ErrorT RuntimeError IO a }

-- IO (Error RuntimeError a)

deriving instance MonadIO FP
deriving instance MonadFix FP
deriving instance MonadError RuntimeError FP

instance Monad FP where
  return = FP . return
  (FP a) >>= b = FP (a >>= fmap unFP b)

instance Applicative FP where
  pure = return
  (<*>) = ap

instance Functor FP where
  fmap f (FP x) = FP $ fmap f x

runFP :: FP a -> IO (Either RuntimeError a)
runFP = runErrorT . unFP
