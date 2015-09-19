{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Common types for the FP15 compiler.
module FP15.Compiler.Types (
  module FP15.Compiler.Types
, module FP15.Compiler.Lookup
, module FP15.Compiler.Modules
) where
import Control.Monad
import Control.Applicative
import FP15.Types
import qualified Data.Map.Strict as M
import Data.List.NonEmpty(NonEmpty(..))
import FP15.Compiler.Lookup
import FP15.Compiler.Modules

-- * Type Synonyms

-- | The function signature of 'FP15.Parsing.Parser.parse'.
type Parser = ModuleSource -> Either String ModuleAST

-- * Reduction

-- | A symbol with import source attached.
type WithImpSrc a = (Located Import, a)

-- | Names that come from imports.
newtype ImportedNames
  = Imported (ModuleBody Name (NE (WithImpSrc AFName))
                              (NE (WithImpSrc AFlName))
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

