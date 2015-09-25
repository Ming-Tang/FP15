{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module FP15.Compiler.Modules where
import Control.Monad
import Control.Applicative
import qualified Data.Map.Strict as M
import FP15.Types
import FP15.Compiler.Lookup

-- | 'ModuleBody' is for creating types that follow the structure of a module,
-- which contains map of functions, map of functionals, and fixity declarations.
-- The representations for the functions and functionals are provided by the
-- type parameters @fn@ and @fl@ respectively.
data ModuleBody id f fl ffx flfx
  = Module { fs :: Map (id F) f
           , fls :: Map (id Fl) fl
           , fFixes :: Map (id FOp) ffx
           , flFixes :: Map (id FlOp) flfx }

deriving instance (Eq (id F), Eq (id Fl), Eq (id FOp), Eq (id FlOp),
                   Eq f, Eq fl, Eq ffx, Eq flfx)
                  => Eq (ModuleBody id f fl ffx flfx)
deriving instance (Ord (id F), Ord (id Fl), Ord (id FOp), Ord (id FlOp),
                   Ord f, Ord fl, Ord ffx, Ord flfx)
                  => Ord (ModuleBody id f fl ffx flfx)
deriving instance (Show (id F), Show (id Fl), Show (id FOp), Show (id FlOp),
                   Show f, Show fl, Show ffx, Show flfx)
                  => Show (ModuleBody id f fl ffx flfx)
deriving instance (Ord (id F), Ord (id Fl), Ord (id FOp), Ord (id FlOp),
                   Ord f, Ord fl, Ord ffx, Ord flfx,
                   Read (id F), Read (id Fl), Read (id FOp), Read (id FlOp),
                   Read f, Read fl, Read ffx, Read flfx)
                  => Read (ModuleBody id f fl ffx flfx)

-- | A 'ModuleInterface' represents the functions, functional and
-- fixities a module exports.
newtype ModuleInterface
  = ModuleInterface (ModuleBody Id () () FFixity FlFixity)
  deriving (Eq, Ord, Show, Read)

-- | An 'MIContext' represents a 'Lookup' instance of the module relative to
-- itself.
--
-- Within the module @M@, both @n@ and @M.n@ refer to @M.n@.
data MIContext = MIContext ModuleName ModuleInterface
               deriving (Eq, Ord, Show, Read)

instance LookupF MIContext where
  lookupF (MIContext (M m) (ModuleInterface Module { fs })) (N m' n)
    | m == m' || [] == m' = const (N m n) <$> M.lookup (Id n) fs
    | otherwise = Nothing

instance LookupFl MIContext where
  lookupFl (MIContext (M m) (ModuleInterface Module { fls })) (N m' n)
    | m == m' || [] == m' = const (N m n) <$> M.lookup (Id n) fls
    | otherwise = Nothing

instance LookupOp MIContext where
  lookupOp (MIContext (M m) (ModuleInterface Module { fFixes, flFixes })) (N m' o)
    | m == m' || [] == m' = (Right <$> M.lookup (Id o) flFixes)
                            `mplus` (Left <$> M.lookup (Id o) fFixes)
    | otherwise = Nothing

instance Lookup MIContext where

-- | A 'SourceMapping' gives the source location of for all aspects of a module
-- interface. If the key is absent, then the source mapping for the specified
-- key is absent.
newtype SourceMapping
  = SourceMapping (ModuleBody Id SrcPos SrcPos SrcPos SrcPos)
  deriving (Eq, Ord, Show, Read)
