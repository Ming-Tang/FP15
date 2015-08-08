{-# LANGUAGE ViewPatterns #-}
-- | Module for reducing expressins from 'ExprAST' to 'BExpr'.
module FP15.Compiler.Reduction.BExpr where
import Control.Monad.Error
import Control.Monad.Trans.Reader
import Control.Applicative
import FP15.Types
import FP15.Compiler.Types
import FP15.Value
import FP15.Compiler.Precedence

infixl 6 |>
infixr 6 <|

-- * Types

type BResult e a = ReaderT e (Either BError) a

type FTree = Tree (ResolvedOp F) ExprAST
type FlTree = Tree (ResolvedOp Fl) ExprAST

-- | A 'BError' is an error that can occur while converting an 'ExprAST' into a
-- 'BExpr'.
data BError
  -- | Generic error with message and maybe the location of error.
  = ErrorMsg !String !(Maybe ExprAST)

  -- | The specified operator cannot be resolved.
  | OpNotFound !(LocName Unknown)

  -- | A precedence parsing error has occurred while parsing
  -- a '()'-expression.
  | PrecErrorF !(PrecParseError (ResolvedOp F) ExprAST)

  -- | A precedence parsing error has occurred while parsing
  -- a '{}'-expression.
  | PrecErrorFl !(PrecParseError (ResolvedOp Fl) ExprAST)

  -- | Functional operators are not allowed in @()@-expressions.
  -- The argument indicates the offending functional operator.
  | FlOpNotAllowed !(ResolvedOp Fl)

  -- | Partial operators are not allowed in @{}@-expresions.
  -- The argument indicates the partially applied operator and its
  -- operand. The value is described as follows:
  --
  --  * @Nothing@ means the location of error is not provided.
  --
  --  * @Just (Left (o, b))@ means the left operand is missing.
  --    @o@ is the operator and @b@ is the right operand.
  --
  --  * @Just (Right (a, o))@ means the right operand is missing.
  --    @o@ is the operator and @a@ is the left operand.
  | FlPartialOpNotAllowed !(Maybe (Either (ResolvedOp Fl, FlTree)
                                          (FlTree, ResolvedOp Fl)))
  deriving (Eq, Ord, Show, Read)

-- | An 'ResolvedOp' represents an operator (with location of mention) that has
-- been successfully resolved to an identifier.
data ResolvedOp f = ResolvedOp { getOp :: !(LocName Unknown)
                               , getResolvedId :: !(Name f) }
                  deriving (Eq, Ord, Show, Read)

getLocResolvedId :: ResolvedOp f -> Located (Name f)
getLocResolvedId (ResolvedOp (Loc l _) i) = Loc l i

-- * Functions

-- | The 'convExprAST' function converts an 'ExprAST' into a 'BExpr'.
convExprAST :: LookupOp e => e -> ExprAST -> Either BError BExpr
convExprAST env ast = runReaderT (toBE ast) env

toBE :: LookupOp e => ExprAST -> BResult e BExpr
toBE (TValue v) = return $ BConst v
toBE (TFunc f) = return $ BFunc f
toBE (TOperator o) = do
  (Loc _ (Fixity _ _ f)) <- lookupFOpOnly o
  return $ BFunc $ withSameLoc o f
toBE (TDotOperator f) = return $ BFunc f
toBE (TApp fl es) = BApp fl <$> mapM toBE es
toBE (TIndex i) = base "Index" <*> pure [BConst $ Int 1]

toBE (TIf p a b) = base "If" <*> mapM toBE [p, a, b]
toBE (TFork es) = base "Fork" <*> mapM toBE es
toBE (TPass es) = base "Pass" <*> mapM toBE es

toBE (TUnresolvedPrimaryList ps) =
  toPrecNodesFl ps
  >>= lift . conv PrecErrorFl . parsePrec . insDefault composeOp
  >>= fromTreeFl where
  -- TODO move this as a constant
  composeOp :: (Assoc, Prec, ResolvedOp Fl)
  composeOp = (VarA, (0, 0), ResolvedOp boCompose $ getLocated bnCompose)

toBE (TUnresolvedInfixNotation ps) =
  toPrecNodesF ps
  >>= lift . conv PrecErrorF . parsePrec
  >>= fromTreeF

toBE (TLet _ _) = error "FP15.Compiler.Reduction.convExprAST: TLet is not implemented."

toPrecNodesFl :: LookupOp e => [ExprAST] -> BResult e [PrecNode (ResolvedOp Fl) ExprAST]
toPrecNodesFl = mapM toPrecNodeFl

toPrecNodeFl :: LookupOp e => ExprAST -> BResult e (PrecNode (ResolvedOp Fl) ExprAST)
toPrecNodeFl (TOperator o@(Loc l _)) = do
  o' <- lookupOpOnly o
  case o' of
    Left (Fixity _ _ fa) -> return $ TermN $ TFunc $ withSameLoc o fa
    Right f -> return $ precNodeFromFixity o $ Loc l f

toPrecNodeFl (TDotOperator f) = return $ TermN $ TFunc f
toPrecNodeFl x = return $ TermN x

fromTreeFl :: LookupOp e => Tree (ResolvedOp Fl) ExprAST -> BResult e BExpr
fromTreeFl (Term Nothing) = throwError $ FlPartialOpNotAllowed Nothing
fromTreeFl (Term (Just x)) = toBE x
fromTreeFl (Pre (getLocResolvedId -> o) x) = (BApp o . (:[])) <$> fromTreeFl x
fromTreeFl (Inf x ro@(getLocResolvedId -> o) y)
  = o2 <$> ce le (fromTreeFl x) <*> ce re (fromTreeFl y) where
  o2 a b = BApp o [a, b]
  (le, re) = (Left (ro, y), Right (x, ro))
  ce c = (`catchError` refineE c)
  refineE e (FlPartialOpNotAllowed Nothing)
    = throwError $ FlPartialOpNotAllowed $ Just e
  refineE _ e = throwError e
fromTreeFl (Var (getLocResolvedId -> o) xs) = BApp o <$> mapM fromTreeFl xs

toPrecNodesF :: LookupOp e => [ExprAST] -> BResult e [PrecNode (ResolvedOp F) ExprAST]
toPrecNodesF = mapM toPrecNodeF

toPrecNodeF :: LookupOp e => ExprAST -> BResult e (PrecNode (ResolvedOp F) ExprAST)
toPrecNodeF (TDotOperator o)
  = return $ InfN LeftA (-5, 0) $ ResolvedOp (fmap convName o) (getLocated o)
toPrecNodeF (TOperator o) = precNodeFromFixity o <$> lookupFOpOnly o
toPrecNodeF x = return $ TermN x

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

precNodeFromFixity :: LocName Unknown -> Located (Fixity f)
                    -> PrecNode (ResolvedOp f) a
precNodeFromFixity o o'@(Loc _ (Fixity typ p oa)) =
  case typ of
    Prefix -> PreN p $ ResolvedOp o oa
    LeftAssoc -> InfN LeftA p $ ResolvedOp o oa
    RightAssoc -> InfN RightA p $ ResolvedOp o oa
    VarAssoc -> InfN VarA p $ ResolvedOp o oa

-- * Lookups

lookupFOpOnly :: LookupOp e => LocName Unknown -> BResult e (LocFixity F)
lookupFOpOnly o = do
  e <- ask
  case lookupOp e $ getLocated o of
    Nothing -> throwError $ OpNotFound o
    Just (Right (Fixity _ _ a)) -> throwError $ FlOpNotAllowed $ ResolvedOp o a
    Just (Left f) -> return $ withSameLoc o f

lookupOpOnly :: LookupOp e => LocName Unknown
                -> BResult e (Either FFixity FlFixity)
lookupOpOnly o = do
  e <- ask
  case lookupOp e $ getLocated o of
    Nothing -> throwError $ OpNotFound o
    Just f -> return f

-- * Primitive Symbols

base :: Monad m => String -> m ([BExpr] -> BExpr)
base = return . BApp . baseName

baseName :: String -> LocName f
baseName = Loc Nothing . N ["Std"]

pId :: BExpr
pId = BFunc $ baseName "_"

boCompose :: LocName Unknown
boCompose = baseName ""

bnCompose, bnFork :: LocName Fl
bnCompose = baseName "Compose"
bnFork = baseName "Fork"

bFork :: [BExpr] -> BExpr
bFork = BApp bnFork

bC2 :: BExpr -> BExpr -> BExpr
bC2 a b = BApp bnCompose [a, b]

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
