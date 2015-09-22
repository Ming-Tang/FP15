{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
module FP15.Compiler.Lookup where
import Control.Monad
import qualified Data.Map.Strict as M
import FP15.Types

type OpResult = Either FFixity FlFixity

-- These typeclasses are for symbol lookups. There are three namespaces for
-- symbols: function, functional and operator. Notice that operators for
-- functions and functionals share the same namespace.

class LookupF e where
  lookupF :: e -> RFName -> Maybe (FName Abs)

class LookupFl e where
  lookupFl :: e -> RFlName -> Maybe (FlName Abs)

class LookupOp e where
  lookupOp :: e -> RUName -> Maybe OpResult

class (LookupF e, LookupFl e, LookupOp e) => Lookup e where

instance LookupF (M.Map RFName AFName) where
  lookupF = flip M.lookup

instance LookupFl (M.Map RFlName AFlName) where
  lookupFl = flip M.lookup

instance LookupOp (M.Map RUName OpResult) where
  lookupOp = flip M.lookup

-- * Lookup Helpers

-- | The 'EmptyLookup' is a dummy implementation for the lookup typeclasses. It
-- returns 'Nothing' for all inputs.
data EmptyLookup = EmptyLookup deriving (Eq, Ord, Enum, Bounded, Show, Read)

instance LookupF EmptyLookup where
  lookupF _ _ = Nothing
  {-# SPECIALIZE INLINE lookupF :: EmptyLookup -> RFName -> Maybe (FName Abs) #-}

instance LookupFl EmptyLookup where
  lookupFl _ _ = Nothing
  {-# SPECIALIZE INLINE lookupFl :: EmptyLookup -> RFlName -> Maybe (FlName Abs) #-}

instance LookupOp EmptyLookup where
  lookupOp _ _ = Nothing
  {-# SPECIALIZE INLINE lookupOp :: EmptyLookup -> RUName -> Maybe OpResult #-}

-- | The 'FallbackLookup' chains two lookup instances. When looking up from a
-- @(FallbackLookup a b)@, the lookup is attempted from @a@, and if it fails,
-- @b@ is attempted.
data FallbackLookup a b = Fallback a b deriving (Eq, Ord, Show, Read)

_fb :: (a -> n -> Maybe c) -> (b -> n -> Maybe c)
       -> FallbackLookup a b -> n -> Maybe c
_fb lo lo' (Fallback a b) n = lo a n `mplus` lo' b n

instance (LookupF a, LookupF b) => LookupF (FallbackLookup a b) where
  lookupF = _fb lookupF lookupF
  {-# SPECIALIZE INLINE lookupF :: (LookupF a, LookupF b) => FallbackLookup a b -> RFName -> Maybe (FName Abs) #-}

instance (LookupFl a, LookupFl b) => LookupFl (FallbackLookup a b) where
  lookupFl = _fb lookupFl lookupFl
  {-# SPECIALIZE INLINE lookupFl :: (LookupFl a, LookupFl b) => FallbackLookup a b -> RFlName -> Maybe (FlName Abs) #-}

instance (LookupOp a, LookupOp b) => LookupOp (FallbackLookup a b) where
  lookupOp = _fb lookupOp lookupOp
  {-# SPECIALIZE INLINE lookupOp :: (LookupOp a, LookupOp b) => FallbackLookup a b -> RUName -> Maybe OpResult #-}

instance (Lookup a, Lookup b) => Lookup (FallbackLookup a b) where

-- ** Existential Types

-- | The 'AnyLookup' type encapsulates an instance of 'Lookup'.
data AnyLookup = forall e. Lookup e => AnyLookup e

-- | The 'AnyLookupF' type encapsulates an instance of 'LookupF'.
data AnyLookupF = forall e. LookupF e => AnyLookupF e

-- | The 'AnyLookupFl' type encapsulates an instance of 'LookupFl'.
data AnyLookupFl = forall e. LookupFl e => AnyLookupFl e

-- | The 'AnyLookupOp' type encapsulates an instance of 'LookupOp'.
data AnyLookupOp = forall e. LookupOp e => AnyLookupOp e
