{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module FP15.Evaluator.FP where
import Control.Applicative
import Control.Monad.RWS.Strict
import Control.Monad.Error
import FP15.Evaluator.FPEnv(FPEnv(..), initial)
import FP15.Evaluator.RuntimeError

type R = FPEnv
type W = ()
type S = ()

initR :: R
initR = initial

initS :: S
initS = ()

-- | The 'FP' monad is the monad for executing FP15 code. It contains the following behaviors:
--
-- 1. Side effects ('IO')
-- 2. Read-only context (@RWST FPEnv@)
-- 3. Error handling (@ErrorT RuntimeError@)
--
-- Since 'ErrorT' is the outermost monad transformer in the transformer stack,
-- the context is available even if an error has occurred. See the signature of
-- 'runFP' for more details.
newtype FP a = FP { unFP :: ErrorT RuntimeError (RWST R W S IO) a }

deriving instance MonadIO FP
deriving instance MonadFix FP
deriving instance MonadError RuntimeError FP
deriving instance MonadReader R FP
deriving instance MonadWriter W FP
deriving instance MonadState S FP
deriving instance MonadRWS R W S FP

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
execFP fp = (\(a, _, _) -> a) <$> runFP initR initS fp

getEnv :: FP R
getEnv = ask

withEnv :: (R -> R) -> FP a -> FP a
withEnv = local

getClosure :: FP (R, S)
getClosure = (,) <$> getEnv <*> get
