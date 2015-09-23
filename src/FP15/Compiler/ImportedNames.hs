module FP15.Compiler.ImportedNames where
import FP15.Types
import Data.Monoid
import Control.Applicative
import Control.Monad(mplus)
import qualified Data.Map.Strict as M
import FP15.Compiler.Modules
import FP15.Compiler.ModuleBody as MB
import FP15.Compiler.Lookup
import Data.List.NonEmpty(NonEmpty((:|)))

-- | A symbol with import source attached.
type WithImpSrc a = (Located Import, a)

-- | Names that come from imports, which is four mappings from local name to
-- absolute name with import source.
newtype ImportedNames
  = Imported (ModuleBody Name (NE (WithImpSrc AFName))
                              (NE (WithImpSrc AFlName))
                              (NE (WithImpSrc FFixity))
                              (NE (WithImpSrc FlFixity)))
  deriving (Eq, Ord, Show, Read)

empty :: ImportedNames
empty = Imported MB.empty

-- | The 'miToImports' function converts the module interface of a module of
-- given name into an 'ImportedNames' with names processed by a function.
miToImportedNames
  :: ([String] -> String -> ([String], String))
     -- ^ Function that processes the key of the import, which returns the
     -- the two components for a 'Name'.
     -> Located Import -- ^ The import for this module.
     -> ModuleName -- ^ The name of the module the MI corresponds to.
     -> ModuleInterface -- ^ The module interface of the imported module.
     -> ImportedNames
miToImportedNames nameMapping li (M m) (ModuleInterface mi)
  = Imported $ MB.map (mapKV mkName withLIM,
                       mapKV mkName withLIM,
                       mapKV mkName $ const withLI,
                       mapKV mkName $ const withLI) mi
    where mkName (Id i) = uncurry N $ nameMapping m i
          withLI x = si (li, x)
          withLIM (Id i) () = si (li, N m i)
          mapKV mk mv = M.mapKeys mk . M.mapWithKey mv
          si = (:|[]) :: a -> NE a

-- | The 'mergeImportedNames' function merges the imports from two functions.
mergeImportedNames :: ImportedNames -> ImportedNames -> ImportedNames
mergeImportedNames (Imported a) (Imported b)
  = Imported $ MB.combine (mergeML, mergeML, mergeML, mergeML) a b
  where mergeML :: Ord k => Map k (NE v) -> Map k (NE v) -> Map k (NE v)
        mergeML = M.unionWith appendNE
        appendNE :: NE a -> NE a -> NE a
        appendNE (x:|xs) (y:|ys) = x :| xs ++ y:ys

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

instance Monoid ImportedNames where
  mempty = FP15.Compiler.ImportedNames.empty
  mappend = mergeImportedNames
