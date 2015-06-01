{-# LANGUAGE EmptyDataDecls, StandaloneDeriving #-}
module FP15.Types where
import FP15.Value(Value)
import qualified Data.Map.Strict as M
import qualified Data.List.NonEmpty as NE

-- * Type Synonyms

-- Short identifiers are needed because of 80-char line length limit.

-- | A strict map.
type Map k v = M.Map k v

-- | A strict set.
type Set e = Map e ()

-- | A non-empty list
type NE a = NE.NonEmpty a

-- * Names

-- | An identifier of any kind (function, functional, operator)
newtype Id a = Id String
             deriving (Eq, Ord, Show, Read)

-- | An FP15 module name.
newtype ModuleName = M [String]
                   deriving (Eq, Ord, Show, Read)

getId (Id i) = i
getModuleName (M n) = n

-- | A fully-qualified name
data Name a = N ![String] !String
            deriving (Eq, Ord, Show, Read)

-- * Source Position

-- | The type of source positions, which locates the part of a source file by
-- position, line and column, with optional filename.
data SrcPos = SrcPos { position :: !Int
                     , line :: !Int
                     , column :: !Int
                     , file :: !(Maybe String) }
            deriving (Eq, Ord, Show, Read)

-- | A value with optional location information attached.
data Located a = Loc !(Maybe SrcPos) a
               deriving (Eq, Ord, Show, Read)

getSrcPos :: Located a -> Maybe SrcPos
getLocated :: Located a -> a

getSrcPos (Loc spos _) = spos
getLocated (Loc _ x) = x

type LocId a = Located (Id a)
type LocName a = Located (Name a)

-- * Type Tags

data F
data Fl
data FOp
data FlOp
data Unknown

deriving instance Eq F
deriving instance Ord F
deriving instance Show F
deriving instance Read F

deriving instance Eq Fl
deriving instance Ord Fl
deriving instance Show Fl
deriving instance Read Fl

deriving instance Eq FOp
deriving instance Ord FOp
deriving instance Show FOp
deriving instance Read FOp

deriving instance Eq FlOp
deriving instance Ord FlOp
deriving instance Show FlOp
deriving instance Read FlOp

deriving instance Eq Unknown
deriving instance Ord Unknown
deriving instance Show Unknown
deriving instance Read Unknown

-- ** Type Aliases

type FId = Id F
type FlId = Id Fl
type FOpId = Id FOp
type FlOpId = Id FlOp

type FName = Name F
type FlName = Name Fl
type FOpName = Name FOp
type FlOpName = Name FlOp

type UnknownName = Name Unknown


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
             | TUnary (LocName FOp) ExprAST
             | TBinary ExprAST (LocName FOp) ExprAST
             | TIndex Int

             | TIf ExprAST ExprAST ExprAST
             | TFork [ExprAST]
             | TPass [ExprAST]

             | TUnresolvedPrimaryList [ExprAST]
             | TUnresolvedInfixNotation [ExprAST]

             | TLet [(LocId F, ExprAST)] ExprAST

deriving instance Eq ExprAST
deriving instance Ord ExprAST
deriving instance Show ExprAST
deriving instance Read ExprAST


data ExprState = Unresolved ExprAST
               | Unreduced Expr
               | Reduced Expr
               deriving (Eq, Ord, Show, Read)

-- TODO types for derived functionals

-- * Compilation

data ModuleSource = ModuleSource { moduleFile :: !(Maybe String)
                                 , moduleSource :: !String }
                  deriving (Eq, Ord, Show, Read)

data ModuleResolutionResult
  = ModuleResolution { resolutionErrors :: Map ModuleName ModuleResolutionError
                     , resolvedModuleASTs :: Map ModuleName ModuleAST }
  deriving (Eq, Ord, Show, Read)

data ModuleResolutionError = ModuleNotFound
                           | ParseError String
                           deriving (Eq, Ord, Show, Read)

data PreparationError = ImportCycle { modulesInCycle :: [ModuleName] }
                      | UndefinedReference

-- * Custom operators
type Prec = (Int, Int)

data OperatorAlias a = AsIs | AliasOf (Name a)
                     deriving (Eq, Ord, Show, Read)

data OperatorType = Prefix | LeftAssoc | RightAssoc
                  deriving (Eq, Ord, Show, Read)

data Fixity a = Fixity OperatorType Prec (OperatorAlias a)
              deriving (Eq, Ord, Show, Read)

type FixityDecls = ([Fixity F], [Fixity Fl])

-- * ASTs

type FunctionalAST = ()

type FunctionalDefinition = ()

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

data ModuleAST = ModuleAST { astFs :: Map (LocId F) ExprAST
                           , astFls :: Map (LocId Fl) FunctionalAST
                           , astFFixes :: Map (LocId FOp) FFixity
                           , astFlFixes :: Map (LocId FlOp) FlFixity
                           , astImps :: ImportList
                           , astExps :: ExportList }
               deriving (Eq, Ord, Show, Read)
