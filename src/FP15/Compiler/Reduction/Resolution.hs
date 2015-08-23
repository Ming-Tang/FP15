module FP15.Compiler.Reduction.Resolution (
  RResult
, ResolutionError(..)
, resolveExpr
) where
import Text.PrettyPrint
import Control.Monad.Error
import Control.Monad.Trans.Reader
import Control.Applicative((<$>), (<*>))
import FP15.Types
import FP15.Compiler.Types

type RResult e a = ReaderT e (Either ResolutionError) a

data ResolutionError = FNotFound (LocName F)
                     | FlNotFound (LocName Fl)
                     deriving (Eq, Ord, Show, Read)

instance Disp ResolutionError where
  pretty (FNotFound f) = text "Function not found:" <+> pretty f
  pretty (FlNotFound f) = text "Functional not found:" <+> pretty f

-- | The 'resolveExpr' function resolves all names inside an 'Expr' to
-- fully-qualified names.
resolveExpr :: Lookup e => e -> Expr -> Either ResolutionError Expr
resolveExpr env expr = runReaderT (resolve expr) env

resolve :: Lookup e => Expr -> RResult e Expr
resolve (Const x) = return $ Const x
resolve (App f xs) = App <$> loFl f <*> mapM resolve xs
resolve (Func f) = Func <$> loF f

loFl :: Lookup e => LocName Fl -> RResult e (LocName Fl)
loF :: Lookup e => LocName F -> RResult e (LocName F)
lo :: Lookup e => (LocName f -> ResolutionError)
                  -> (e -> Name f -> Maybe (Name f))
                  -> LocName f -> RResult e (LocName f)

lo re lf lx@(Loc l x) = do
  e <- ask
  case lf e x of
    Just y -> return $ Loc l y
    Nothing -> throwError $ re lx

loFl = lo FlNotFound lookupFl
loF = lo FNotFound lookupF
