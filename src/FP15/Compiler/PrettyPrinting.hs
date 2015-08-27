module FP15.Compiler.PrettyPrinting where
import Text.PrettyPrint
import FP15.Types
import FP15.Compiler.Types
import qualified Data.Map.Strict as M

-- | The 'prettyCMILines' function returns a list of a 'CompiledModuleItem''s
-- declarations, each pretty-printed individually.
prettyCMILines
  :: ModuleName -- ^ The prefixing module name.
  -> CompiledModuleItem -> [Doc]
prettyCMILines
  (M m) (CompiledModuleItem { cmiCM = (Compiled Module { fs = cmfs }) }) =
  map (\(Id f, e) -> pretty (N m f) <+> text "=" <+> pretty e) $ M.toList cmfs

