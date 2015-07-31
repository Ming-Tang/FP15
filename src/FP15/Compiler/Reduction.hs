{-# LANGUAGE ViewPatterns #-}
module FP15.Compiler.Reduction where
import Control.Monad.Error
import Control.Monad.Trans.Reader
import Control.Applicative
import FP15.Types
import FP15.Value
import FP15.Compiler.Precedence

infixl 6 |>
infixr 6 <|

class LookupF e where
  lookupF :: e -> Name F -> Maybe (Located (LocName F))

class LookupFl e where
  lookupFl :: e -> Name Fl -> Maybe (Located (LocName F))

class LookupOp e where
  lookupOp :: e -> Name Unknown -> Maybe (Either (LocFixity F) (LocFixity Fl))

class (LookupF e, LookupFl e, LookupOp e) => Lookup e where

instance Error BError where
  strMsg s = ErrorMsg s Nothing

type BResult e a = ReaderT e (Either BError) a

data BError = NoMsg !String
            | ErrorMsg !String !(Maybe ExprAST)
            | OpNotFound !(LocName Unknown)

            | PrecErrorF !(PrecParseError (ResolvedOp F) ExprAST)
            | PrecErrorFl !(PrecParseError (ResolvedOp Fl) ExprAST)

            | FlOpNotAllowed !(LocFixity Fl) !(LocName Unknown)
            | FlPartialOpNotAllowed !(LocName FlOp)
            deriving (Eq, Ord, Show, Read)

-- | An 'ResolvedOp' represents an operator (located) that has been successfully
-- resolved to an identifier.
data ResolvedOp f = ResolvedOp { getOp :: !(LocName Unknown)
                               , getResolvedId :: !(Name f) }
                  deriving (Eq, Ord, Show, Read)
getLocResolvedId :: ResolvedOp f -> Located (Name f)
getLocResolvedId (ResolvedOp (Loc l _) i) = Loc l i

-- | Given an ExprAST, convert it to BExpr
convExprAST :: LookupOp e => e -> ExprAST -> Either BError BExpr
convExprAST env ast = runReaderT (toBE ast) env

toBE :: LookupOp e => ExprAST -> BResult e BExpr
toBE (TValue v) = return $ BConst v
toBE (TFunc f) = return $ BFunc f
toBE o@(TOperator f) = throwError $ ErrorMsg ("TOperator " ++ show f) (Just o)
toBE o@(TDotOperator f) = throwError $ ErrorMsg ("TDotOperator " ++ show f) (Just o)
toBE (TApp fl es) = BApp fl <$> mapM toBE es
toBE (TIndex i) = base "Index" <*> pure [BConst $ Int 1]

toBE (TIf p a b) = base "If" <*> mapM toBE [p, a, b]
toBE (TFork es) = base "Fork" <*> mapM toBE es
toBE (TPass es) = base "Pass" <*> mapM toBE es

toBE (TUnresolvedPrimaryList ps) =
  toPrecNodesFl ps
  >>= lift . conv PrecErrorFl . parsePrec . insDefault composeOp
  >>= fromTreeFl where
  composeOp :: (Assoc, Prec, ResolvedOp Fl)
  composeOp = (LeftA, (0, 0), ResolvedOp boCompose $ getLocated bnCompose)

toBE (TUnresolvedInfixNotation ps) =
  toPrecNodesF ps
  >>= lift . conv PrecErrorF . parsePrec
  >>= fromTreeF

toBE (TLet _ _) = error "FP15.Compiler.Reduction.convExprAST: TLet"

toPrecNodesFl :: LookupOp e => [ExprAST] -> BResult e [PrecNode (ResolvedOp Fl) ExprAST]
toPrecNodesFl xs = return $ map (const undefined) xs

toPrecNodeFl :: LookupOp e => ExprAST -> BResult e (PrecNode (ResolvedOp Fl) ExprAST)
toPrecNodeFl (TOperator o@(Loc l _)) = do
  o' <- lookupOpOnly o
  case o' of
    Left (Loc _ (Fixity _ _ fa)) -> return $ TermN $ TFunc $ Loc l fa
    Right (Loc _ (Fixity typ p oa)) ->
      case typ of
        Prefix -> return $ PreN p $ ResolvedOp o oa
        LeftAssoc -> return $ InfN LeftA p $ ResolvedOp o oa
        RightAssoc -> return $ InfN RightA p $ ResolvedOp o oa
        VarAssoc -> return $ InfN VarA p $ ResolvedOp o oa

toPrecNodeFl (TDotOperator f) = return $ TermN $ TFunc f
toPrecNodeFl x = return $ TermN x

fromTreeFl :: LookupOp e => Tree (ResolvedOp Fl) ExprAST -> BResult e BExpr
fromTreeFl (Term Nothing) = throwError $ ErrorMsg "Missing operand in {}-expression." Nothing
fromTreeFl _ = undefined

toPrecNodesF :: LookupOp e => [ExprAST] -> BResult e [PrecNode (ResolvedOp F) ExprAST]
toPrecNodesF xs = return undefined

fromTreeF :: LookupOp e => Tree (ResolvedOp F) ExprAST -> BResult e BExpr
fromTreeF (Pre (getLocResolvedId -> p) x) = do
  a <- fromTreeF x
  return $ a |> BFunc p
fromTreeF (Inf x (getLocResolvedId -> o) y) = do
  (a, b) <- (,) <$> fromTreeF x <*> fromTreeF y
  return $ bFork [a, b] |> BFunc o
fromTreeF (Var (getLocResolvedId -> o) xs) = do
  xs' <- mapM fromTreeF xs
  return $ bFork xs' |> BFunc o
fromTreeF (Term (Just x)) = toBE x
fromTreeF (Term Nothing) = return pId

-- * Lookups

lookupFOpOnly :: LookupOp e => LocName Unknown -> BResult e (LocFixity F)
lookupFOpOnly n = do
  e <- ask
  case lookupOp e $ getLocated n of
    Nothing -> throwError $ OpNotFound n
    Just (Right o) -> throwError $ FlOpNotAllowed o n
    Just (Left o) -> return o

lookupOpOnly :: LookupOp e => LocName Unknown
                -> BResult e (Either (LocFixity F) (LocFixity Fl))
lookupOpOnly n = do
  e <- ask
  case lookupOp e $ getLocated n of
    Nothing -> throwError $ OpNotFound n
    Just o -> return o

-- * Primitive Symbols

base :: Monad m => String -> m ([BExpr] -> BExpr)
base = return . BApp . baseName

baseName :: String -> LocName f
baseName = Loc Nothing . N ["Std"]

pId :: BExpr
pId = BFunc $ baseName "id"

boCompose :: LocName Unknown
boCompose = baseName ""

bnCompose, bnFork :: LocName Fl
bnCompose = baseName "Compose"
bnFork = baseName "Fork"

bFork :: [BExpr] -> BExpr
bFork = BApp bnFork

bC2 :: BExpr -> BExpr -> BExpr
bC2 _ _ = undefined

(|>) :: BExpr -> BExpr -> BExpr
(|>) = bC2

(<|) :: BExpr -> BExpr -> BExpr
(<|) = flip bC2

-- * Error Handling

conv :: (a -> c) -> Either a b -> Either c b
conv f = either (Left . f) Right

nToL :: a -> Maybe b -> Either a b
nToL x Nothing = Left x
nToL _ (Just y) = Right y
