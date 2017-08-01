{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
module FP15.Expr where
import GHC.Generics(Generic)
import Control.DeepSeq
import Text.PrettyPrint
import Data.Void(Void)
import FP15.Xtn
import FP15.Disp
import FP15.Name
import FP15.Value

-- * Expressions

data XExpr f fl x = Const Value
                  | App fl [XExpr f fl x]
                  | Func f
                  | Get Int
                  | With (XExpr f fl x) (XExpr f fl x)
                  | Pop (XExpr f fl x)
                  | Ex !x

deriving instance (Eq x, Eq f, Eq fl) => Eq (XExpr f fl x)
deriving instance (Ord x, Ord f, Ord fl) => Ord (XExpr f fl x)
deriving instance (Show x, Show f, Show fl) => Show (XExpr f fl x)
deriving instance (Read x, Read f, Read fl) => Read (XExpr f fl x)
deriving instance (Generic x, Generic f, Generic fl) => Generic (XExpr f fl x)
deriving instance Functor (XExpr f fl)
instance (NFData x, NFData f, NFData fl) => NFData (XExpr f fl x) where rnf x = seq x ()

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
  pretty (Get i) = pretty (TGet i)
  pretty (With x e) = text "#<with>" <> lbrace <> pretty x <> rbrace <+> pretty e
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

             | TPop ExprAST
             | TWith ExprAST
             | TWithLeft ExprAST
             | TWithRight ExprAST
             | TGet !Int

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

  pretty (TWith e) = text "#=" <+> lbrace <> pretty e <> rbrace
  pretty (TWithLeft e) = text "#<" <+> lbrace <> pretty e <> rbrace
  pretty (TWithRight e) = text "#>" <+> lbrace <> pretty e <> rbrace
  pretty (TPop e) = text "#-" <+> lbrace <> pretty e <> rbrace

  pretty (TGet 0) = text "#^"
  pretty (TGet 1) = text "#^^"
  pretty (TGet 2) = text "#^^^"
  pretty (TGet i) = text "#^" <> text (show i)

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

type FunctionalAST = ()
