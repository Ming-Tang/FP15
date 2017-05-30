{-# LANGUAGE DeriveGeneric #-}
module FP15.Modules where
import Text.PrettyPrint
import GHC.Generics(Generic)
import Control.DeepSeq
import Data.Map.Strict(Map)
import FP15.Disp
import FP15.Name
import FP15.Expr

-- * Type Synonyms
type FFixity = Fixity F
type FlFixity = Fixity Fl

-- * Compilation

-- TODO belongs elsewhere
-- TODO moduleSource should be Maybe
data ModuleSource = ModuleSource { moduleFile :: !(Maybe String)
                                 , moduleSource :: !String }
                  deriving (Eq, Ord, Show, Read, Generic)

instance NFData ModuleSource where rnf x = seq x ()

data ModuleResolutionError = ModuleNotFound
                           | ParseError String
                           deriving (Eq, Ord, Show, Read, Generic)

instance NFData ModuleResolutionError where rnf x = seq x ()

-- * Custom Operators

-- | An operator precedence is a pair of 'Int's, ordered by the first element,
-- then by the second element.
type Prec = (Int, Int)

data OperatorType = Prefix | LeftAssoc | RightAssoc | VarAssoc
                  deriving (Eq, Ord, Show, Read, Generic)

instance NFData OperatorType where rnf x = seq x ()

-- | Fixity declaration.
data Fixity a = Fixity OperatorType Prec (Name (a, Abs))
              deriving (Eq, Ord, Show, Read, Generic)

instance NFData (Fixity a) where rnf x = seq x ()

type LocFixity a = Located (Fixity a)

-- TODO why is this here?
type FunctionalDefinition = ()

-- | The 'MIRef' type represents a reference to an item from a module interface.
-- This type is used to express lookups, imports, and exports.
data MIRef f fl op = MIF f | MIFl fl | MIOp op
                   deriving (Eq, Ord, Show, Read, Generic)

instance (NFData f, NFData fl, NFData op) => NFData (MIRef f fl op) where rnf x = seq x ()

-- | A selective import.
type SelImp = MIRef (Id F) (Id Fl) (Id Unknown)

newtype ModRename = ModRename ModuleName
                  deriving (Eq, Ord, Show, Read, Generic)

instance NFData ModRename where rnf x = seq x ()

instance Disp ModRename where
  pretty (ModRename m) = pretty m <+> text "=" <> space

data ImpQual = Unqual | Qual
               deriving (Eq, Ord, Show, Read, Generic)

instance NFData ImpQual where rnf x = seq x ()

instance Disp ImpQual where
  pretty Unqual = text ""
  pretty Qual = text "."

-- An import filter.
data ImpFilters = Selective ![SelImp] | Hiding ![SelImp]
            deriving (Eq, Ord, Show, Read, Generic)

instance NFData ImpFilters where rnf x = seq x ()

instance Disp ImpFilters where
  pretty (Selective _) = text "(...)"
  pretty (Hiding _) = text "-(...)"

-- An import statement.
data Import = Import { impModule :: !ModuleName
                     , impQual :: !ImpQual
                     , impRename :: !(Maybe ModRename)
                     , impFilters :: !(Maybe ImpFilters) }
            deriving (Eq, Ord, Show, Read, Generic)

mkImport, mkImportQual :: ModuleName -> Import
mkImport m = Import m Unqual Nothing Nothing
mkImportQual m = Import m Qual Nothing Nothing

instance Disp Import where
  pretty (Import m q r f)
    = text "+" <> mp r <> pretty m <> pretty q <> mp f where
      mp :: Disp a => Maybe a -> Doc
      mp = maybe empty pretty

-- +Module            import Module
-- +Module.           import qualified Module
-- +M = Module        import Module as M
-- +M = Module.       import qualified Module as M

-- +Module(a)         import Module(a)
-- +Module.(a)        import qualified Module(a)
-- +M = Module(a)     import Module(a) as M
-- +M = Module.(a)    import qualified Module(a) as M

-- +Module-(a)        import Module hiding (a)

instance NFData Import where rnf x = seq x ()

type ImportList = [Located Import]

type ExportList = [Located SelImp]

-- ** AST

data ModuleAST = ModuleAST { astMN :: !ModuleName
                           , astImps :: !ImportList
                           , astExps :: !ExportList
                           , astFs :: Map (LocId F) ExprAST
                           , astFls :: Map (LocId Fl) FunctionalAST
                           , astFFixes :: Map (LocId FOp) FFixity
                           , astFlFixes :: Map (LocId FlOp) FlFixity
                           }
               deriving (Eq, Ord, Show, Read, Generic)

instance NFData ModuleAST where rnf x = seq x ()

