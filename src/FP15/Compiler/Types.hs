{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Common types for the FP15 compiler.
module FP15.Compiler.Types (
  ImportedNames(..)
, module FP15.Compiler.Types
, module FP15.Compiler.Lookup
, module FP15.Compiler.Modules
) where
import GHC.Generics
import Control.DeepSeq
import FP15.Types
import FP15.Compiler.Lookup
import FP15.Compiler.ImportedNames as IN
import FP15.Compiler.Modules

-- * Type Synonyms

-- | The function signature of 'FP15.Parsing.Parser.parse'.
type Parser = ModuleSource -> Either String ModuleAST

-- * Names

-- | The 'UnName' type represents an unresolved name.
data UnName f
  -- | A name relative to an environment.
  = RN (RName f)
  -- | A name that requies the syntax provider of the enviornment to resolve.
  | SN String
  -- ^ An name meant to be absolute regardless of context.
  | AN (AName f)
  deriving (Eq, Ord, Show, Read, Generic)

instance NFData f => NFData (UnName f) where

-- | The 'EnvName' type represents a name relative to an environment @e@.
data EnvName f e
  -- | A name relative to an environment.
  = REN e (RName f)
  -- ^ An absolute name.
  | AEN e (AName f)
  deriving (Eq, Ord, Show, Read, Generic, Functor)

-- TODO the env doesn't need to lookup infix operators

instance (NFData e, NFData f) => NFData (EnvName e f) where

type LocUnName f = Located (UnName f)

-- * Expression Types

-- | A desugared FP15 expression.
type BExpr = XExpr (LocUnName F) (LocUnName Fl) ()

-- * Reduction

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

-- | A module that is being reduced.
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
               | Unreduced BExpr
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

