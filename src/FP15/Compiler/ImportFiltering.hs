module FP15.Compiler.ImportFiltering where
import Prelude hiding (lookup, filter)
import qualified Prelude as P
import Data.Maybe(isNothing)
import Control.Monad(mplus)
import qualified Data.Map.Strict as M
import FP15.Types
import FP15.Compiler.Modules
import qualified FP15.Compiler.ModuleBody as MB

-- | The 'lookup' function lookups the value of a selective import item.
lookup :: SelImp -> ModuleInterface -> Maybe (MIRef () () (Either FFixity FlFixity))
lookup (MIF f) (ModuleInterface Module { fs }) = fmap MIF (M.lookup f fs)
lookup (MIFl fl) (ModuleInterface Module { fls }) = fmap MIF (M.lookup fl fls)
lookup (MIOp (Id op)) (ModuleInterface Module { fFixes, flFixes }) =
  fmap MIOp (a `mplus` b) where
    a = fmap Left (M.lookup (Id op) fFixes)
    b = fmap Right (M.lookup (Id op) flFixes)

-- | The 'filter' function produces a new 'ModuleInterface' with the specified
-- import filters applied, or an error with list of unresolved names.
filter :: ImpFilters -> ModuleInterface -> Either [SelImp] ModuleInterface
filter f mi@(ModuleInterface mi') =
  case f of  -- TODO finish this
    Selective ns -> es ns (ModuleInterface $ MB.map (mkf ns MIF, mkf ns MIFl, mkf ns undefined, mkf ns undefined) mi')
    Hiding ns -> undefined
  where es ns g = case P.filter (isNothing . (`lookup` mi)) ns of
                    [] -> Right g
                    xs -> Left xs
        mkf ns c = M.filterWithKey (\k _ -> elem (c k) ns)
