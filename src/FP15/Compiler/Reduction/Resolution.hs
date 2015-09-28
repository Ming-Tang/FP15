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

data ResolutionError = FNotFound (RLocName F)
                     | FlNotFound (RLocName Fl)
                     deriving (Eq, Ord, Show, Read)

instance Disp ResolutionError where
  pretty (FNotFound f) = text "Function not found:" <+> pretty f
  pretty (FlNotFound f) = text "Functional not found:" <+> pretty f

-- | The 'resolveExpr' function resolves all names inside an 'Expr' to
-- fully-qualified names.
resolveExpr :: Lookup e => e -> BExpr -> Either ResolutionError Expr
resolveExpr env expr = runReaderT (resolve expr) env

resolve :: Lookup e => BExpr -> RResult e Expr
resolve (Const x) = return $ Const x
resolve (App f xs) = App <$> loFl f <*> mapM resolve xs
resolve (Func f) = Func <$> loF f
resolve (Get i) = return $ Get i -- TODO validate?
resolve (With e) = With <$> resolve e
resolve (Ex _) = error "resolve: Ex"

-- TODO this need to be rewritten when we have EnvName phase in the future

loFl :: Lookup e => LocUnName Fl -> RResult e (ALocName Fl)
loF :: Lookup e => LocUnName F -> RResult e (ALocName F)
lo :: Lookup e => (RLocName f -> ResolutionError)
                  -> (e -> RName f -> Maybe (AName f)) -- ^ lookup function
                  -> LocUnName f -> RResult e (ALocName f)

lo _ _ (Loc l (AN n)) = return $ Loc l n
lo re lf (Loc l (SN n)) = lo re lf (Loc l (RN $ N synProvider n))
lo re lf lx@(Loc l (RN x)) = do
  e <- ask
  case lf e x of
    Just y -> return $ Loc l y
    Nothing -> throwError $ re $ Loc l x

loFl = lo FlNotFound lookupFl
loF = lo FNotFound lookupF

-- | The 'synProvider' is the relative module name of the syntax provider.
synProvider :: [String]
synProvider = ["$"]
-- TODO get module redirection working and use $ instead.
