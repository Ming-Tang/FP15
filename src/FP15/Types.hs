{-# LANGUAGE Safe #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
module FP15.Types (
  module FP15.Xtn
, module FP15.Disp
, module FP15.Name
, module FP15.Value
, module FP15.Types
, Void
, Map
) where
import GHC.Generics(Generic)
import Control.DeepSeq
import Text.PrettyPrint
import Data.Map.Strict(Map)
import qualified Data.List.NonEmpty as NE
import Data.Void(Void)
import FP15.Xtn
import FP15.Disp
import FP15.Name
import FP15.Value

-- * Type Synonyms
-- Short identifiers are needed because of 80-char line length limit.

-- | A strict set.
type Set e = Map e ()

-- | A non-empty list.
type NE a = NE.NonEmpty a

type FFixity = Fixity F
type FlFixity = Fixity Fl

-- * Expressions

data XExpr f fl x = Const Value
                  | App fl [XExpr f fl x]
                  | Func f
                  | Ex !x

deriving instance (Eq x, Eq f, Eq fl) => Eq (XExpr f fl x)
deriving instance (Ord x, Ord f, Ord fl) => Ord (XExpr f fl x)
deriving instance (Show x, Show f, Show fl) => Show (XExpr f fl x)
deriving instance (Read x, Read f, Read fl) => Read (XExpr f fl x)
deriving instance (Generic x, Generic f, Generic fl) => Generic (XExpr f fl x)
deriving instance Functor (XExpr f fl)
instance (NFData x, NFData f, NFData fl) => NFData (XExpr f fl x) where

instance Xtn (XExpr f fl) where
  maybeX (Ex x) = Just x
  maybeX _ = Nothing
  fromX = Ex

prettyApp :: Doc -> [Doc] -> Doc
prettyApp f xs = lparen <> fsep (map (nest 2) (f:xs)) <> rparen

instance (Disp f, Disp fl, Disp x) => Disp (XExpr f fl x) where
  pretty (Const v) = pretty v
  pretty (App f xs) = prettyApp (pretty f) $ map pretty xs
  pretty (Func f) = pretty f
  pretty (Ex x) = pretty x

-- | An FP15 expression with the specified name type.
type Expr = XExpr (LocName F Abs) (LocName Fl Abs) Void

-- | An FP15 expression as seen by the parser.
data ExprAST = TValue Value
             | TFunc (RLocName F)
             | TOperator (RLocName Unknown)
             | TDotOperator (RLocName F)
             | TApp (RLocName Fl) [ExprAST]
             | TIndex Int
             | TId

             | TIf ExprAST ExprAST ExprAST
             | TFork [ExprAST]
             | THook [ExprAST]

             | TUnresolvedPrimaryList [ExprAST]
             | TUnresolvedInfixNotation [ExprAST]
             | TUnresolvedCommaNotation [Either Int ExprAST]

             | TLet [(LocId F, ExprAST)] ExprAST

deriving instance Eq ExprAST
deriving instance Ord ExprAST
deriving instance Show ExprAST
deriving instance Read ExprAST

instance Disp ExprAST where
  -- TODO prim/non-prim contexts
  pretty (TValue v) = pretty v
  pretty (TFunc (Loc _ f)) = pretty f
  pretty (TOperator (Loc _ o)) = pretty o
  pretty (TDotOperator (Loc _ o)) = pretty o
  pretty (TApp (Loc _ f) xs) =
    lparen <> fsep (map (nest 2 . pretty) xs) <> rparen
  pretty (TIndex i) = text ("#" ++ show i)
  pretty TId = text "_"

  pretty (TIf p a b)
    = lbrace <> pretty p <> text ":" <> pretty a
             <> text "|" <> pretty b <> rbrace
  pretty (TFork xs) =
    -- XXX copy paste of FP15.Value: instance Disp Value
    -- TODO extract function
    lbrack <> fsep (punctuate comma $ map (nest 2 . pretty) xs) <> rbrack
  pretty (THook xs) =
    lbrace <> fsep (punctuate comma $ map (nest 2 . pretty) xs) <> rbrace
  pretty (TUnresolvedPrimaryList xs) =
    lbrace <> fsep (map (nest 2 . pretty) xs) <> rbrace
  pretty (TUnresolvedInfixNotation xs) =
    lparen <> fsep (map (nest 2 . pretty) xs) <> rparen
  pretty (TUnresolvedCommaNotation xs) =
    lparen <> fsep (map (nest 2 . dec) xs) <> rparen where
    dec (Left i) = text (replicate i ',')
    dec (Right e) = pretty e

  pretty (TLet ds e) =
    text "#: [...]" <+> lbrace <> pretty e <> rbrace

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
data Fixity a = Fixity OperatorType Prec (Name (a, Abs))
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
            | ImportRename { importModule :: ModuleName
                           , rename :: String }
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
