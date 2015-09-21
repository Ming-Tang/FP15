-- | The 'FP15.Xtn' module captures the type extension design pattern.
module FP15.Xtn where
import Data.Maybe(isJust)
import Data.Either(isRight)
import Control.Monad.Identity(Identity(..))

-- | The 'Xtn' typeclass is for datatypes with a case dedicated for type
-- extension.
--
-- Laws of 'Xtn':
--
--   @maybeX . fromX = Just@
--   @convX (fromX x) = fromX (f x)@
--   @isX = isJust . maybeX@
--   @fmap f . maybeX = maybeX . convX f@
--   @(maybeX a == Just x && maybeX b == Just x) <==> (a == b)@
--
-- The 5th law states that there is only one possible 'fromX'.
--
-- TODO maybe use 'Control.Functor.Pointed'?
class Functor t => Xtn t where
  {-# MINIMAL maybeX, fromX #-}
  -- | The 'maybeX' function returns 'Just' of the extension of the value is an
  -- extension. Otherwise, return 'Nothing'.
  maybeX :: t x -> Maybe x

  -- | The 'isX' function determines if a value has an Xtn case.
  isX :: t x -> Bool
  isX = isJust . maybeX
  {-# INLINE isX #-}

  -- | The 'fromX' function must be defined to be the extension case.
  fromX :: x -> t x

  -- | The 'convX' function converts from an 'Xtn' container of one type @x@ to
  -- another one @y@.
  convX :: (x -> y) -> t x -> t y
  convX = fmap
  {-# INLINE convX #-}

instance Xtn Identity where
  maybeX = Just . runIdentity
  isX _ = True
  fromX = Identity

instance Xtn Maybe where
  maybeX = id
  isX = isJust
  fromX = Just

instance Xtn (Either a) where
  maybeX (Right x) = Just x
  maybeX _ = Nothing
  isX = isRight
  fromX = Right

