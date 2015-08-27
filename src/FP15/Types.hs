{-# LANGUAGE Safe #-}
{-# LANGUAGE StandaloneDeriving #-}
module FP15.Types (
  module FP15.Disp
, module FP15.Name
, module FP15.Types
) where
import Text.PrettyPrint
import FP15.Disp
import FP15.Name
import FP15.Value
import qualified Data.Map.Strict as M
import qualified Data.List.NonEmpty as NE

-- * Type Synonyms
-- Short identifiers are needed because of 80-char line length limit.

-- | A strict map.
type Map k v = M.Map k v

-- | A strict set.
type Set e = Map e ()

-- | A non-empty list.
type NE a = NE.NonEmpty a

type FFixity = Fixity F
type FlFixity = Fixity Fl

-- * Expressions

-- | An FP15 expression with the specified name type.
data Expr = Const Value
          | App (LocName Fl) [Expr]
          | Func (LocName F)

deriving instance Eq Expr
deriving instance Ord Expr
deriving instance Show Expr
deriving instance Read Expr

prettyApp :: Doc -> [Doc] -> Doc
prettyApp f xs = lparen <> fsep (map (nest 2) (f:xs)) <> rparen

instance Disp Expr where
  pretty (Const v) = pretty v
  pretty (App (Loc _ f) xs) = prettyApp (pretty f) $ map pretty xs
  pretty (Func (Loc _ f)) = pretty f

-- | An FP15 expressions with local bindings.
data BExpr = BConst Value
           | BApp (LocName Fl) [BExpr]
           | BFunc (LocName F)
           | BLet [(LocId F, BExpr)] BExpr

deriving instance Eq BExpr
deriving instance Ord BExpr
deriving instance Show BExpr
deriving instance Read BExpr

-- | An FP15 expression as seen by the parser.
data ExprAST = TValue Value
             | TFunc (LocName F)
             | TOperator (LocName Unknown)
             | TDotOperator (LocName F)
             | TApp (LocName Fl) [ExprAST]
             | TIndex Int
             | TId

             | TIf ExprAST ExprAST ExprAST
             | TFork [ExprAST]
             | THook [ExprAST]

             | TUnresolvedPrimaryList [ExprAST]
             | TUnresolvedInfixNotation [ExprAST]

             | TLet [(LocId F, ExprAST)] ExprAST

deriving instance Eq ExprAST
deriving instance Ord ExprAST
deriving instance Show ExprAST
deriving instance Read ExprAST

-- TODO types for derived functionals

-- * Compilation

data ModuleSource = ModuleSource { moduleFile :: !(Maybe String)
                                 , moduleSource :: !String }
                  deriving (Eq, Ord, Show, Read)

data ModuleResolutionError = ModuleNotFound
                           | ParseError String
                           deriving (Eq, Ord, Show, Read)

-- * Custom Operators

-- | An operator precedence is a pair of 'Int's, ordered by the first element,
-- then by the second element.
type Prec = (Int, Int)

data OperatorType = Prefix | LeftAssoc | RightAssoc | VarAssoc
                  deriving (Eq, Ord, Show, Read)

-- | Fixity declaration.
data Fixity a = Fixity OperatorType Prec (Name a)
              deriving (Eq, Ord, Show, Read)

type LocFixity a = Located (Fixity a)

-- * ASTs

type FunctionalAST = ()

type FunctionalDefinition = ()

-- | TODO finish these
data ImpId = A
           deriving (Eq, Ord, Show, Read)
data ImpRename = B
               deriving (Eq, Ord, Show, Read)

-- TODO function/functional/operator
data SelectiveImport = NoRename ImpId
                     | Rename ImpRename
                     deriving (Eq, Ord, Show, Read)

data Import = Import { importModule :: ModuleName }
            | ImportQualified { importModule :: ModuleName }
            | ImportQualifiedRename { importModule :: ModuleName
                                    , qualifiedRename :: String }
            | ImportFrom { importModule :: ModuleName
                         , selectiveImports :: [SelectiveImport] }
            deriving (Eq, Ord, Show, Read)

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
               deriving (Eq, Ord, Show, Read)
