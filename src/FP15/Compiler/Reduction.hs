module FP15.Compiler.Reduction where
import Data.List(foldl')
import Control.Monad.Error
import Control.Monad.Trans.Reader
import Control.Applicative
import FP15.Types
import FP15.Value

type Env = ()

class LookupF e where
  lookupF :: e -> Name F -> Maybe ()

class LookupFl e where
  lookupFl :: e -> Name Fl -> Maybe ()

class LookupOp e where
  lookupOp :: e -> Maybe (Either () ())

class (LookupF e, LookupFl e, LookupOp e) => Lookup e where {}

-- | Given an ExprAST, convert it to BExpr
convExprAST :: LookupOp e => e -> ExprAST -> Either String BExpr
convExprAST env ast = runReaderT (toBE ast) env

extractOps :: [ExprAST] -> [Either () [ExprAST]]
extractOps = foldl' f []
  where f acc x = acc

toBE :: LookupOp e => ExprAST -> ReaderT e (Either String) BExpr
toBE (TValue v) = return $ BConst v
toBE (TFunc f) = return $ BFunc f
toBE (TOperator f) = throwError $ "TOperator " ++ show f
toBE (TApp fl es) = BApp fl <$> mapM toBE es
toBE (TIndex i) = base "Index" <*> pure [BConst $ Int 1]
toBE (TIf p a b) = base "If" <*> mapM toBE [p, a, b]
toBE (TFork es) = base "Fork" <*> mapM toBE es
toBE (TPass es) = base "Pass" <*> mapM toBE es
toBE (TLet _ _) = error "FP15.Compiler.Reduction.convExprAST: TLet"
toBE ast = throwError ""

base :: Monad m => String -> m ([BExpr] -> BExpr)
base f = return $ BApp $ Loc Nothing $ N ["Std"] f

