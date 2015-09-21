-- | The 'FP15.Xtn' module captures the type extension design pattern.
module FP15.Xtn where
import Data.Maybe(isJust)

-- | The 'Xtn' typeclass is for datatypes with a case dedicated for type
-- extension.
--
-- Laws of 'Xtn':
--
--   @maybeX . fromX = Just@
--   @convX (fromX x) = fromX (f x)@
--   @isX = isJust . maybeX@
--   @fmap f . maybeX = maybeX . convX f@
--
class Xtn t where
  -- | The 'maybeX' function returns 'Just' of the extension of the value is an
  -- extension. Otherwise, return 'Nothing'.
  maybeX :: t x -> Maybe x

  -- | The 'isX' function determines if a value has an Xtn case.
  isX :: t x -> Bool
  isX = isJust . maybeX

  -- | The 'fromX' function must be defined to be the extension case.
  fromX :: x -> t x

  -- | The 'convX' function converts from an 'Xtn' container of one type @x@ to
  -- another one @y@.
  convX :: (x -> y) -> t x -> t y

