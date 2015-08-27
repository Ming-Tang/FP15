{-# LANGUAGE StandaloneDeriving, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
-- | Common types for the FP15 compiler.
module FP15.Compiler.Types where
import Control.Monad
import Control.Applicative
import FP15.Types
import qualified Data.Map.Strict as M
import Data.List.NonEmpty(NonEmpty(..))

-- * The Lookup Typeclasses
-- These typeclasses are for symbol lookups. There are three namespaces for
-- symbols: function, functional and operator. Notice that operators for
-- functions and functionals share the same namespace.

class LookupF e where
  lookupF :: e -> FName -> Maybe FName

class LookupFl e where
  lookupFl :: e -> FlName -> Maybe FlName

class LookupOp e where
  lookupOp :: e -> Name Unknown -> Maybe (Either FFixity FlFixity)

class (LookupF e, LookupFl e, LookupOp e) => Lookup e where

instance LookupF (M.Map FName FName) where
  lookupF = flip M.lookup

instance LookupFl (M.Map FlName FlName) where
  lookupFl = flip M.lookup

instance LookupOp (M.Map (Name Unknown) (Either FFixity FlFixity)) where
  lookupOp = flip M.lookup

-- ** Lookup Helpers

-- | The 'EmptyLookup' is a dummy implementation for the lookup typeclasses. It
-- returns 'Nothing' for all inputs.
data EmptyLookup = EmptyLookup deriving (Eq, Ord, Enum, Bounded, Show, Read)

instance LookupF EmptyLookup where
  lookupF _ _ = Nothing
  {-# SPECIALIZE INLINE lookupF :: EmptyLookup -> FName -> Maybe FName #-}

instance LookupFl EmptyLookup where
  lookupFl _ _ = Nothing
  {-# SPECIALIZE INLINE lookupFl :: EmptyLookup -> FlName -> Maybe FlName #-}

instance LookupOp EmptyLookup where
  lookupOp _ _ = Nothing
  {-# SPECIALIZE INLINE lookupOp :: EmptyLookup -> Name Unknown -> Maybe (Either FFixity FlFixity) #-}

-- | The 'FallbackLookup' chains two lookup instances. When looking up from a
-- @(FallbackLookup a b)@, the lookup is attempted from @a@, and if it fails,
-- @b@ is attempted.
data FallbackLookup a b = Fallback a b deriving (Eq, Ord, Show, Read)

_fb :: (a -> n -> Maybe c) -> (b -> n -> Maybe c)
       -> FallbackLookup a b -> n -> Maybe c
_fb lo lo' (Fallback a b) n = lo a n `mplus` lo' b n

instance (LookupF a, LookupF b) => LookupF (FallbackLookup a b) where
  lookupF = _fb lookupF lookupF
  {-# SPECIALIZE INLINE lookupF :: (LookupF a, LookupF b) => FallbackLookup a b -> FName -> Maybe FName #-}

instance (LookupFl a, LookupFl b) => LookupFl (FallbackLookup a b) where
  lookupFl = _fb lookupFl lookupFl
  {-# SPECIALIZE INLINE lookupFl :: (LookupFl a, LookupFl b) => FallbackLookup a b -> FlName -> Maybe FlName #-}

instance (LookupOp a, LookupOp b) => LookupOp (FallbackLookup a b) where
  lookupOp = _fb lookupOp lookupOp
  {-# SPECIALIZE INLINE lookupOp :: (LookupOp a, LookupOp b) => FallbackLookup a b -> Name Unknown -> Maybe (Either FFixity FlFixity) #-}

instance (Lookup a, Lookup b) => Lookup (FallbackLookup a b) where

-- * Type Synonyms

-- | The function signature of 'FP15.Parsing.Parser.parse'.
type Parser = ModuleSource -> Either String ModuleAST

-- * Module Body

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

-- * Reduction

-- | A symbol with import source attached.
type WithImpSrc a = (Located Import, a)

-- | Names that come from imports.
newtype ImportedNames
  = Imported (ModuleBody Name (NE (WithImpSrc FName))
                              (NE (WithImpSrc FlName))
                              (NE (WithImpSrc FFixity))
                              (NE (WithImpSrc FlFixity)))
  deriving (Eq, Ord, Show, Read)

-- | Variables in the reduction process that do not change.
data StaticState
  = SS { ssCMS :: CompiledModuleSet
         -- ^ The modules that are already compiled
       , ssMN :: ModuleName
         -- ^ The name of the module being compiled
       , ssMI :: ModuleInterface
         -- ^ The module interface of the module being compiled
       , ssSM :: Maybe SourceMapping
         -- ^ The source mappings of names in this module
       , ssIN :: ImportedNames
         -- ^ The names imported by this module
       }
  deriving (Eq, Ord, Show, Read)

instance LookupF StaticState where
  lookupF SS { ssMN, ssIN, ssMI } = lookupF (Fallback ssIN (MIContext ssMN ssMI))

instance LookupFl StaticState where
  lookupFl SS { ssMN, ssIN, ssMI } = lookupFl (Fallback ssIN (MIContext ssMN ssMI))

instance LookupOp StaticState where
  lookupOp SS { ssMN, ssIN, ssMI } = lookupOp (Fallback ssIN (MIContext ssMN ssMI))

instance Lookup StaticState where

_gf :: NE (WithImpSrc a) -> a
_gf ((_, f) :| _) = f

instance LookupF ImportedNames where
  lookupF (Imported Module { fs }) e = _gf <$> M.lookup (convName e) fs

instance LookupFl ImportedNames where
  lookupFl (Imported Module { fls }) e = _gf <$> M.lookup (convName e) fls

instance LookupOp ImportedNames where
  lookupOp (Imported Module { fFixes, flFixes }) e
    = f Right flFixes `mplus` f Left fFixes where
      f c b = (c . _gf) <$> M.lookup (convName e) b

instance Lookup ImportedNames where

-- | A module that is being reduced.
--
-- The @dependencyGraph@ field contains a graph representing the "is depended
-- by" relationship between functions, as an adjacency map. The edge @a -> b@
-- denotes the function @b@ depends on @a@.
newtype ReducingModule
  = Reducing (ModuleBody Id ExprState FunctionalDefinition FFixity FlFixity)
  deriving (Eq, Ord, Show, Read)

-- | The state of the reducing module.
data ReducingTag
  = Normal               -- ^ The module is being reduced and is not blocked.
  | Blocked [ModuleName] -- ^ Waiting for modules to be compiled.
  | Finished             -- ^ Module has finished reducing.
                 deriving (Eq, Ord, Show, Read)
data ReducingModuleState
  = ReducingModuleState { rmsSS :: StaticState
                        , rmsTag :: ReducingTag
                        , rmsRM :: ReducingModule }
  deriving (Eq, Ord, Show, Read)

-- | An 'ExprState' represents an expression in various stages of the reduction
-- stage.
--
-- TODO error states.
data ExprState = Unresolved ExprAST
               | Unlifted BExpr
               | Unreduced Expr
               | Reduced Expr
               deriving (Eq, Ord, Show, Read)

-- * Compiled

-- | A 'CompiledModule' represents a module with functions in compiled state.
newtype CompiledModule
  = Compiled (ModuleBody Id Expr FunctionalDefinition FFixity FlFixity)
  deriving (Eq, Ord, Show, Read)

data CompiledModuleItem
  = CompiledModuleItem { cmiCM :: CompiledModule
                       , cmiMI :: ModuleInterface
                       , cmiSM :: Maybe SourceMapping }
  deriving (Eq, Ord, Show, Read)

-- | A 'CompiledModuleSet' represents a set of compiled modules with module
-- interface and source mapping (optional) as additional information.
newtype CompiledModuleSet
  = CompiledModuleSet (Map ModuleName CompiledModuleItem)
  deriving (Eq, Ord, Show, Read)

getCompiledModule
  :: CompiledModule -> ModuleBody Id Expr FunctionalDefinition FFixity FlFixity
getReducingModule
  :: ReducingModule
     -> ModuleBody Id ExprState FunctionalDefinition FFixity FlFixity
getCompiledModuleSet :: CompiledModuleSet -> Map ModuleName CompiledModuleItem

getReducingModule (Reducing r) = r
getCompiledModule (Compiled c) = c
getCompiledModuleSet (CompiledModuleSet c) = c

