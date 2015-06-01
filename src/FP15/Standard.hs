module FP15.Standard where
import qualified Data.Map.Strict as M
import FP15.Types
import FP15.Compiler.Types

stdFs = map Id ["_", "succ", "pred", "isEven", "isOdd"]
stdFls = map Id ["Compose", "If", "Fork", "Pass"]
stdFFixes = []
stdFlFixes = []

set :: Ord a => [a] -> Map a ()
set = M.fromList . map (\n -> (n, ()))

stdMI = ModuleInterface Module {
          fs = set stdFs
        , fls = set stdFls
        , fFixes = M.fromList stdFFixes
        , flFixes = M.fromList stdFlFixes
        }

stdCompiledModule = Compiled Module {
                      fs = M.empty
                    , fls = M.empty
                    , fFixes = M.fromList stdFFixes
                    , flFixes = M.fromList stdFlFixes
                    }

stdModuleItem = CompiledModuleItem stdCompiledModule stdMI Nothing

standardCMS = CompiledModuleSet (M.fromList [(M ["Std"], stdModuleItem)])

