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

-- | Fixity declaration.
data Fixity a = Fixity OperatorType Prec (Name (a, Abs))
              deriving (Eq, Ord, Show, Read, Generic)

type LocFixity a = Located (Fixity a)

-- * ASTs

type FunctionalDefinition = ()

-- | TODO finish these
data ImpId = A deriving (Eq, Ord, Show, Read, Generic)
data ImpRename = B deriving (Eq, Ord, Show, Read, Generic)

-- TODO function/functional/operator
data SelectiveImport = NoRename ImpId
                     | Rename ImpRename
                     deriving (Eq, Ord, Show, Read, Generic)

data Import = Import { importModule :: ModuleName }
            | ImportRename { importModule :: ModuleName
                           , rename :: String }
            | ImportQualified { importModule :: ModuleName }
            | ImportQualifiedRename { importModule :: ModuleName
                                    , qualifiedRename :: String }
            | ImportFrom { importModule :: ModuleName
                         , selectiveImports :: [SelectiveImport] }
            deriving (Eq, Ord, Show, Read, Generic)

type ImportList = [Located Import]

type ExportList = [Located ImpId]

-- ** AST

data ModuleAST = ModuleAST { astMN :: ModuleName
                           , astImps :: ImportList
                           , astExps :: ExportList
                           , astFs :: Map (LocId F) ExprAST
                           , astFls :: Map (LocId Fl) FunctionalAST
                           , astFFixes :: Map (LocId FOp) FFixity
                           , astFlFixes :: Map (LocId FlOp) FlFixity
                           }
               deriving (Eq, Ord, Show, Read, Generic)

