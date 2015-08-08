{-# LANGUAGE CPP, EmptyDataDecls, StandaloneDeriving, DeriveFunctor #-}
module FP15.Types where
#define SPECIAL_SHOW 1
import FP15.Value(Value)
import qualified Data.Map.Strict as M
import qualified Data.List.NonEmpty as NE
#if SPECIAL_SHOW
import Data.List(intercalate)
#endif

-- * Type Synonyms
-- Short identifiers are needed because of 80-char line length limit.

-- | A strict map.
type Map k v = M.Map k v

-- | A strict set.
type Set e = Map e ()

-- | A non-empty list.
type NE a = NE.NonEmpty a

-- * Names

-- | An identifier of any kind (function, functional, operator)
newtype Id a = Id String
             deriving (Eq, Ord, Show, Read)

-- | An FP15 module name.
newtype ModuleName = M [String]
#if SPECIAL_SHOW
            deriving (Eq, Ord, Read)

instance Show ModuleName where
  show (M m) = show $ intercalate "." m
#else
            deriving (Eq, Ord, Show, Read)
#endif

getId :: Id a -> String
getModuleName :: ModuleName -> [String]

getId (Id i) = i
getModuleName (M n) = n

-- | A fully-qualified name
data Name a = N ![String] !String
#if SPECIAL_SHOW
            deriving (Eq, Ord, Read)

instance Show (Name a) where
  show (N [] x) = show x
  show (N m x) = show (intercalate "." $ m ++ [x])
#else
            deriving (Eq, Ord, Show, Read)
#endif

convName :: Name a -> Name b
convName (N a b) = N a b

-- * Bound Names

-- | An unique identifier.
type UId = Integer

-- | A name that can potentially be bound. Bound variables are known to have
-- different meanings despite same appearance, therefore, an unique identifier,
-- @UId@ is attached to each bound variable. Two bound variables are equal if
-- and only if their @(UId, n)@ pair are equal. Comparing by @UId@ alone would
-- make this datatype more difficult to work with. That means, @BS 1 "x"@ is not
-- same as @BS 1 "y"@.
--
-- @UId@s come from unique name generators and unique name generators should
-- never reuse an @UId@, to minimize confusion.
--
-- There are two kinds of bound variables in this datatype: binding source, or
-- @BS@, which introduces a binding to a subexpression, and bound variable,
-- or @BV@, which uses an existing binding.
--
-- Here is an example usage of 'Bound'. Consider the FP15 expression
-- @f (:[g=h;h=g] (:[h=k] h) h)@. It would have the following names in terms of
-- @Bound String@, listing from left to right:
--
-- @
--   f: UB "f"    -- f is not bound
--
--   g: BS 0 "g"  -- local definition of g
--   h: BV 1 "h"  -- reference to h in outer scope
--   h: BS 1 "h"  -- local definition of h
--   g: BV 0 "g"  -- reference to h in outer scope
--
--   h: BS 2 "h"  -- local definition of inner h
--   k: UB "k"    -- k is not bound
--   h: BV 2 "h"  -- reference to h in inner scope
--   h: BV 1 "h"  -- reference to h in outer scope
-- @
--
-- When renaming any variables above, the meaning of the expression is preserved
-- if and only if:
--
--   * Any of the @UB@ variables renamed to another distinct @UB@ variable
--   * Any of the @BS@ variable renamed to another @BS@ of distinct @(UId, n)@,
--     and all corresponding @BV@ have their @(UId, n)@ renamed the same way.
data Bound n
  = UB !n -- ^ An unbound (free) name.
  | BS !UId !n -- ^ A binding source: introduces a binding to a subexpr.
  | BV !UId !n -- ^ A bound variable: refers to an existing binding source.
  deriving (Eq, Ord, Show, Read)

-- | State of the unique name generator, which contains the autoincrement
-- counter of next 'UId' and a set of forbidden 'UId's.
type UGenState = (UId, Map UId ())

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
#if SPECIAL_SHOW
               deriving (Eq, Ord, Read, Functor)

instance Show a => Show (Located a) where
  show (Loc _ a) = show a
#else
               deriving (Eq, Ord, Show, Read, Functor)
#endif

getSrcPos :: Located a -> Maybe SrcPos
getLocated :: Located a -> a

getSrcPos (Loc spos _) = spos
getLocated (Loc _ x) = x

noLoc :: a -> Located a
noLoc = Loc Nothing

withSameLoc :: Located a -> b -> Located b
withSameLoc o x = fmap (const x) o

type LocId a = Located (Id a)
type LocName a = Located (Name a)

-- * Type Tags
-- These empty datatypes are for tagging name types, to prevent programming
-- mistakes of passing a function name into a place where a functional name is
-- required.

-- | Function.
data F
-- | Functional.
data Fl
-- | Function operator.
data FOp
-- | Functional operator.
data FlOp
-- | Either function or functional operator.
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
#if SPECIAL_SHOW
instance Show BExpr where
  show (BConst v) = "(BConst " ++ show v ++ ")"
  show (BApp f xs) = "(BApp " ++ show f ++ " " ++ unwords (map show xs) ++ ")"
  show (BFunc f) = "(BFunc " ++ show f ++ ")"
  show (BLet bs x) = "(BLet " ++ show bs ++ " " ++ show x ++ ")"

#else
deriving instance Show BExpr
#endif
deriving instance Read BExpr

-- | An FP15 expression as seen by the parser.
data ExprAST = TValue Value
             | TFunc (LocName F)
             | TOperator (LocName Unknown)
             | TDotOperator (LocName F)
             | TApp (LocName Fl) [ExprAST]
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
