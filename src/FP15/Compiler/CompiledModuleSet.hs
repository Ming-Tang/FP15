module FP15.Compiler.CompiledModuleSet where
import Control.Monad
import FP15.Types
import FP15.Compiler.Types
import qualified Data.Map.Strict as M

-- XXX This looks like I'm exposing an internal module...

empty :: CompiledModuleSet
empty = CompiledModuleSet M.empty

lookupCompiledModule :: CompiledModuleSet -> ModuleName
                        -> Maybe CompiledModuleItem
lookupCompiledModule (CompiledModuleSet m) n = M.lookup n m

lookupModule :: CompiledModuleSet -> ModuleName -> Maybe CompiledModule
lookupModuleInterface :: CompiledModuleSet -> ModuleName
                         -> Maybe ModuleInterface
lookupSourceMapping :: CompiledModuleSet -> ModuleName -> Maybe SourceMapping

(lookupModule, lookupModuleInterface, lookupSourceMapping) =
  (fromLookup getM, fromLookup getMI, \m n -> join $ fromLookup getSM m n)
  where fromLookup g m = lookupCompiledModule m >=> return . g
        getM (CompiledModuleItem m _ _) = m
        getMI (CompiledModuleItem _ mi _) = mi
        getSM (CompiledModuleItem _ _ sm) = sm

addModule :: ModuleName -> CompiledModuleItem -> CompiledModuleSet
             -> CompiledModuleSet
addModule n m (CompiledModuleSet ms) = CompiledModuleSet (M.insert n m ms)
