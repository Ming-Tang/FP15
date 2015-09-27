{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module FP15.Evaluator.FP where
import Control.Applicative
import Control.Monad.RWS.Strict
import Control.Monad.Error
import FP15.Evaluator.RuntimeError

type R = ()
type W = ()
type S = ()

-- | The 'FP' monad is for executing FP15 code. This monad contains 'IO' and
-- error handling ('RuntimeError').
newtype FP a = FP { unFP :: ErrorT RuntimeError (RWST R W S IO) a }

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

runFP :: R -> S -> FP a -> IO (Either RuntimeError a, W, S)
runFP r s = flip (`runRWST` r) s . runErrorT . unFP

execFP :: FP a -> IO (Either RuntimeError a)
execFP fp = (\(a, _, _) -> a) <$> runFP () () fp
