{-# LANGUAGE StandaloneDeriving, FlexibleInstances, FlexibleContexts #-}
module FP15.Compiler.Types where
import FP15.Types
import qualified Data.Map.Strict as M

-- * The Lookup Typeclasses
-- These typeclasses are for symbol lookups. There are three namespaces for
-- symbols: function, functional and operator. Notice that operators for
-- functions and functionals share the same namespace.

class LookupF e where
  lookupF :: e -> Name F -> Maybe (Located (LocName F))

class LookupFl e where
  lookupFl :: e -> Name Fl -> Maybe (Located (LocName Fl))

class LookupOp e where
  lookupOp :: e -> Name Unknown -> Maybe (Either (LocFixity F) (LocFixity Fl))

class (LookupF e, LookupFl e, LookupOp e) => Lookup e where

instance LookupF (M.Map (Name F) (Located (LocName F))) where
  lookupF = flip M.lookup

instance LookupFl (M.Map (Name Fl) (Located (LocName Fl))) where
  lookupFl = flip M.lookup

instance LookupOp (M.Map (Name Unknown) (Either (LocFixity F) (LocFixity Fl))) where
  lookupOp = flip M.lookup

-- ** The Empty Lookup

-- | The 'EmptyLookup' is a dummy implementation for the lookup typeclasses. It
-- returns 'Nothing' for all inputs.
data EmptyLookup = EmptyLookup deriving (Eq, Ord, Enum, Bounded, Show, Read)

instance LookupF EmptyLookup where
  lookupF _ _ = Nothing

instance LookupFl EmptyLookup where
  lookupFl _ _ = Nothing

instance LookupOp EmptyLookup where
  lookupOp _ _ = Nothing

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
       , ssMI :: ModuleInterface
         -- ^ The module interface of the module being compiled
       , ssSM :: Maybe SourceMapping
         -- ^ The source mappings of names in this module
       , ssIN :: ImportedNames
         -- ^ The names imported by this module
       }
  deriving (Eq, Ord, Show, Read)

-- | A module that is being reduced.
--
-- The @dependencyGraph@ field contains a graph representing the "is depended
-- by" relationship between functions, as an adjacency map. The edge @a -> b@
-- denotes the function @b@ depends on @a@.
newtype ReducingModule
  = Reducing (ModuleBody Id ExprState FunctionalDefinition () ())
  deriving (Eq, Ord, Show, Read)

-- | The state of the reducing module.
data ReducingTag
  = Normal               -- ^ The module is being reduced and is not blocked.
  | Blocked [ModuleName] -- ^ Waiting for modules to be compiled.
  | Finished             -- ^ Module has finished reducing.
                 deriving (Eq, Ord, Show, Read)
data ReducingModuleState
  = ReducingModuleState StaticState ReducingTag ReducingModule
  deriving (Eq, Ord, Show, Read)

-- * Compiled

-- | A 'CompiledModule' represents a module with functions in compiled state.
newtype CompiledModule
  = Compiled (ModuleBody Id Expr FunctionalDefinition () ())
  deriving (Eq, Ord, Show, Read)

data CompiledModuleItem
  = CompiledModuleItem CompiledModule ModuleInterface (Maybe SourceMapping)
  deriving (Eq, Ord, Show, Read)

-- | A 'CompiledModuleSet' represents a set of compiled modules with module
-- interface and source mapping (optional) as additional information.
newtype CompiledModuleSet
  = CompiledModuleSet (Map ModuleName CompiledModuleItem)
  deriving (Eq, Ord, Show, Read)

getReducingModule (Reducing r) = r
getCompiledModule (Compiled c) = c
getCompiledModuleSet (CompiledModuleSet c) = c

