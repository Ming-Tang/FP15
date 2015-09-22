{-# LANGUAGE DeriveGeneric #-}
module FP15.Modules where
import GHC.Generics(Generic)
import Control.DeepSeq
import Data.Map.Strict(Map)
import FP15.Name
import FP15.Expr

-- * Type Synonyms
type FFixity = Fixity F
type FlFixity = Fixity Fl

-- * Compilation

data ModuleSource = ModuleSource { moduleFile :: !(Maybe String)
                                 , moduleSource :: !String }
                  deriving (Eq, Ord, Show, Read, Generic)

instance NFData ModuleSource where

data ModuleResolutionError = ModuleNotFound
                           | ParseError String
                           deriving (Eq, Ord, Show, Read, Generic)

instance NFData ModuleResolutionError where

-- * Custom Operators

-- | An operator precedence is a pair of 'Int's, ordered by the first element,
-- then by the second element.
type Prec = (Int, Int)

data OperatorType = Prefix | LeftAssoc | RightAssoc | VarAssoc
                  deriving (Eq, Ord, Show, Read, Generic)

instance NFData OperatorType where

-- | Fixity declaration.
data Fixity a = Fixity OperatorType Prec (Name (a, Abs))
              deriving (Eq, Ord, Show, Read, Generic)

instance NFData (Fixity a) where

type LocFixity a = Located (Fixity a)

-- * ASTs

type FunctionalDefinition = ()

data RNable a = NoRename (Id a) | Rename (Id a) (Id a)
              deriving (Eq, Ord, Show, Read, Generic)

instance NFData (RNable a) where

data SelImp = ImpF (RNable F) | ImpFl (RNable Fl) | ImpOp (Id Unknown)
                     deriving (Eq, Ord, Show, Read, Generic)

instance NFData SelImp where

newtype ModRename = ModRename ModuleName
                  deriving (Eq, Ord, Show, Read, Generic)

instance NFData ModRename where

data ImpQual = Unqual | Qual
               deriving (Eq, Ord, Show, Read, Generic)

data Import = Import { impModule :: !ModuleName
                     , impQual :: !ImpQual
                     , impRename :: !(Maybe ModRename)
                     , impSels :: !(Maybe [SelImp]) }
            deriving (Eq, Ord, Show, Read, Generic)

mkImport, mkImportQual :: ModuleName -> Import
mkImport m = Import m Unqual Nothing Nothing
mkImportQual m = Import m Qual Nothing Nothing

instance NFData Import where

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

