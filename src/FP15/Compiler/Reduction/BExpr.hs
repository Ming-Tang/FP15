{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Module for reducing expressins from 'ExprAST' to 'BExpr'.
module FP15.Compiler.Reduction.BExpr where
import Text.PrettyPrint
import Data.Maybe(isJust)
import Data.List.Split
import Control.Monad.Except
import Control.Monad.Trans.Reader
import Control.Applicative hiding (Const)
import FP15.Value
import FP15.Types
import FP15.Compiler.Types
import FP15.Compiler.Syntax.Precedence
import FP15.Compiler.Syntax.SmartSplit
import FP15.Compiler.Syntax.CommaNotation

infixl 6 |>
infixr 6 <|

-- * Types

type BResult e a = ReaderT e (Either BError) a
type CResult e t o a = ReaderT e (Either (CError o a)) (t o a)

type FTree = Tree (ResolvedOp F) ExprAST
type FlTree = Tree (ResolvedOp Fl) ExprAST

-- | A 'BError' is an error that can occur while converting an 'ExprAST' into a
-- 'BExpr'.
data BError
  -- | Generic error with message and maybe the location of error.
  = ErrorMsg !String !(Maybe ExprAST)

  -- | The specified operator cannot be resolved.
  | OpNotFound !(ULocName Rel)

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

  | CommaError (CError ExprAST ExprAST)
  deriving (Eq, Ord, Show, Read)

withMaybeE :: Doc -> Maybe ExprAST -> Doc
withMaybeE x Nothing = x
withMaybeE x (Just e) = x $+$ text (show e)

instance Disp BError where
  pretty (ErrorMsg msg e) = withMaybeE (pretty msg) e
  pretty (OpNotFound o) = text "Operator not found:" <+> pretty o
  pretty (PrecErrorF e) =
    text "Prec error in ()-expr:" <+> pretty e
  pretty (PrecErrorFl e) =
    text "Prec error in {}-expr:" <+> pretty e
  pretty (FlOpNotAllowed o) =
    text "Functional operator not allowed in ()-expr:" <+> text (show o)
  pretty (FlPartialOpNotAllowed po) =
    text "Missing functional operator operand in {}-expr. <TODO msg>"
  pretty (CommaError ce) = text "Comma notation error: " <+> text (show ce)

-- | An 'ResolvedOp' represents an operator (with location of mention) that has
-- been successfully resolved to an identifier.
data ResolvedOp f = ResolvedOp { getOp :: !(ULocName Rel)
                               , getResolvedId :: !(UnName f) }
                  deriving (Eq, Ord, Show, Read)

instance Same (ResolvedOp f) (RUName, UnName f) where
  key (ResolvedOp (getLocated -> x) n) = (x, n)

getLocResolvedId :: ResolvedOp f -> LocUnName f
getLocResolvedId (ResolvedOp (Loc l _) i) = Loc l i

-- * Functions

-- | The 'convExprAST' function converts an 'ExprAST' into a 'BExpr'.
--
-- The process involves:
--
-- 1. Smart split
-- 2. Comma notation parsing
-- 3. Infix notation parsing
-- 4. Translation
convExprAST :: LookupOp e => e -> ExprAST -> Either BError BExpr
convExprAST env ast = runReaderT (toBE =<< doSmartSplit ast) env

lrn :: Located (RName f) -> Located (UnName f)
lrn (Loc l f) = Loc l $ RN f

mkWith :: LookupOp e => String -> String -> ExprAST -> BResult e BExpr
mkWith sa sb x = do
  x' <- toBE x
  return $ With (baseF sa) (baseF sb |> x')

toBE :: LookupOp e => ExprAST -> BResult e BExpr
toBE TId = return pId
toBE (TValue v) = return $ Const v
toBE (TFunc f) = return $ Func $ lrn f
toBE (TOperator lo@(Loc l o)) = do
  (Loc _ (Fixity _ _ f)) <- lookupFOpOnly lo
  return $ Func $ Loc l $ AN f
toBE (TDotOperator f) = return $ Func $ lrn f

toBE (TPop e) = Pop <$> toBE e
toBE (TWith e) = With pId <$> toBE e
toBE (TWithLeft e) = mkWith "s0" "s1" e
toBE (TWithRight e) = mkWith "s1" "s0" e

-- TODO actually validate index with Reader monad
toBE (TGet i) = return $ Get i

toBE (TApp fl es) = App (lrn fl) <$> mapM toBE es
toBE (TIndex i) =
  return (baseA "Fork" [ pId, Const $ Int $ fromIntegral i]
          |> baseF "index")

toBE (TIf p a b) = base "If" <*> mapM toBE [p, a, b]
toBE (TFork es) = base "Fork" <*> mapM toBE es
toBE (THook es) = base "Hook" <*> mapM toBE es

toBE (TUnresolvedCommaNotation xs) = do
  e <- ask
  -- TODO actually detect operator type
  let isInfixOp = const True
  let ci = toCommaInfix (processInnerInfix, classifyOp) xs
  -- TODO add offset
  let a = fmap convCommaExpr ci
  let b = fmap (insertIndexers isInfixOp TIndex) a
  case b of
    Left ce -> throwError $ CommaError ce
    Right x -> toBE $ uciToExprAST x
    {--- For testing
      error ("FP15.Compiler.Reduction.BExpr.toBE: Unexpected comma notation:\n"
             ++ show ci ++ "\n" ++ show a ++ "\n" ++ show b ++ "\n"
             ++ disp (uciToExprAST x))
    ---}
  where
    processInnerInfix (TUnresolvedInfixNotation xs') = Just (map Right xs')
    processInnerInfix (TUnresolvedCommaNotation xs') = Just xs'
    processInnerInfix _ = Nothing
    classifyOp y = case y of
                     TOperator _ -> Left y
                     TDotOperator _ -> Left y
                     _ -> Right y

toBE (TUnresolvedPrimaryList ps) =
  toPrecNodesFl ps
  >>= lift . conv PrecErrorFl . parsePrec . insDefault composeOp
  >>= fromTreeFl where
  -- TODO move this as a constant
  composeOp :: (Assoc, Prec, ResolvedOp Fl)
  composeOp = (VarA, (0, 0), ResolvedOp (noLoc (N [] "")) $ SN "Compose")

toBE (TUnresolvedInfixNotation ps) =
  toPrecNodesF ps
  >>= lift . conv PrecErrorF . parsePrec
  >>= fromTreeF

toBE (TLet _ _) = error "FP15.Compiler.Reduction.convExprAST: TLet is not implemented."

-- ** Smart Split
doSmartSplit, sst, ss :: LookupOp e => ExprAST -> BResult e ExprAST
doSmartSplit = sst

sst o@(TOperator _) = do
  o' <- ss (TUnresolvedPrimaryList [o])
  return $ case o' of
    TUnresolvedPrimaryList [o''] -> o''
    _ -> o'
sst ast = ss ast

ss (TApp f xs) = TApp f <$> sss xs
ss (TIf p a b) = TIf <$> sst p <*> sst a <*> sst b
ss (TFork xs) = TFork <$> mapM sst xs
ss (THook xs) = THook <$> mapM sst xs
ss (TUnresolvedPrimaryList xs) = TUnresolvedPrimaryList <$> sss xs
ss (TUnresolvedInfixNotation xs) = TUnresolvedInfixNotation <$> sss xs
ss (TUnresolvedCommaNotation xs) = TUnresolvedCommaNotation <$> sss' xs
ss (TLet bs x) = TLet <$> mapM (\(f, y) -> (,) f <$> sst y) bs <*> sst x
ss a = return a

sss :: LookupOp e => [ExprAST] -> BResult e [ExprAST]
sss = (concat <$>) . mapM sso

sss' :: LookupOp e => [Either Int ExprAST] -> BResult e [Either Int ExprAST]
sss' = (concat <$>) . mapM (\x -> case x of Left i -> return [Left i]
                                            Right a -> map Right <$> sso a)

sso :: LookupOp e => ExprAST -> BResult e [ExprAST]
sso o@(TOperator (Loc l (N [] n))) = do
  e <- ask
  return $ case smartSplit (isJust . lookupOp e . N []) n of
    Nothing -> [o]
    Just ps -> map (TOperator . Loc l . N []) ps

sso a = (:[]) <$> ss a

-- ** Comma Notation

toC :: LookupOp e => [Either Int ExprAST] -> CResult e CommaInfix ExprAST ExprAST
toC es = error $ "toC: " ++ show es

toCN :: LookupOp e => ExprAST -> CResult e CommaNode ExprAST ExprAST
toCN e = error $ "toCN: " ++ show e

uciToExprAST :: UncommaInfix ExprAST ExprAST -> ExprAST
uciToExprAST (UInfix _ xs)
  = TUnresolvedInfixNotation $ concatMap ucnToExprAST xs

ucnToExprAST :: UncommaNode ExprAST ExprAST -> [ExprAST]
ucnToExprAST (UComp0 _ c) = c
ucnToExprAST (UComp1 x c) = uciToExprAST x : c
ucnToExprAST (UOp _ o) = [o]

-- ** Precedence Parsing

precNodeFromFixity :: ULocName Rel -> Located (Fixity f)
                    -> PrecNode (ResolvedOp f) a
precNodeFromFixity o o'@(Loc _ (Fixity typ p (AN -> oa))) =
  case typ of
    Prefix -> PreN p $ ResolvedOp o oa
    LeftAssoc -> InfN LeftA p $ ResolvedOp o oa
    RightAssoc -> InfN RightA p $ ResolvedOp o oa
    VarAssoc -> InfN VarA p $ ResolvedOp o oa

-- *** @{}@-Infix Notation

toPrecNodesFl :: LookupOp e => [ExprAST] -> BResult e [PrecNode (ResolvedOp Fl) ExprAST]
toPrecNodesFl = mapM toPrecNodeFl

toPrecNodeFl :: LookupOp e => ExprAST -> BResult e (PrecNode (ResolvedOp Fl) ExprAST)
toPrecNodeFl (TOperator o@(Loc l _)) = do
  o' <- lookupOpOnly o
  case o' of
    Left (Fixity _ _ fa) -> return $ TermN $ TFunc $ Loc l $ convName fa
    Right f -> return $ precNodeFromFixity o $ Loc l f

toPrecNodeFl (TDotOperator f) = return $ TermN $ TFunc f
toPrecNodeFl x = return $ TermN x

fromTreeFl :: LookupOp e => Tree (ResolvedOp Fl) ExprAST -> BResult e BExpr
fromTreeFl (Term Nothing) = throwError $ FlPartialOpNotAllowed Nothing
fromTreeFl (Term (Just x)) = toBE x
fromTreeFl (Pre (getLocResolvedId -> o) x) = (App o . (:[])) <$> fromTreeFl x
fromTreeFl (Inf x ro@(getLocResolvedId -> o) y)
  = o2 <$> ce le (fromTreeFl x) <*> ce re (fromTreeFl y) where
  o2 a b = App o [a, b]
  (le, re) = (Left (ro, y), Right (x, ro))
  ce c = (`catchError` refineE c)
  refineE e (FlPartialOpNotAllowed Nothing)
    = throwError $ FlPartialOpNotAllowed $ Just e
  refineE _ e = throwError e
fromTreeFl (Var (getLocResolvedId -> o) xs) = App o <$> mapM fromTreeFl xs

-- *** @()@-Infix Notation

toPrecNodesF :: LookupOp e => [ExprAST] -> BResult e [PrecNode (ResolvedOp F) ExprAST]
toPrecNodesF = mapM toPrecNodeF . splitPrecNodes
splitPrecNodes :: [ExprAST] -> [ExprAST]
splitPrecNodes = map process . split (dropInitBlank $ dropFinalBlank $ dropInnerBlanks $ whenElt isFOp) where
  isFOp (TDotOperator _) = True
  isFOp (TOperator _) = True
  isFOp _ = False

  process [] = error "splitPrecNodes: empty"
  process [x] | isFOp x = x
  process xs | any isFOp xs = error "toPrecNodesF': consecutive ops"
             | otherwise = TUnresolvedPrimaryList xs

toPrecNodeF :: LookupOp e => ExprAST -> BResult e (PrecNode (ResolvedOp F) ExprAST)
toPrecNodeF (TDotOperator o@(Loc _ o')) = do
  e <- ask
  return $ InfN LeftA (-5, 0) $ ResolvedOp (fmap convName o) $ RN o'
toPrecNodeF (TOperator o) = precNodeFromFixity o <$> lookupFOpOnly o
toPrecNodeF x = return $ TermN x

fromTreeF :: LookupOp e => Tree (ResolvedOp F) ExprAST -> BResult e BExpr
fromTreeF (Pre (getLocResolvedId -> p) x) = do
  a <- fromTreeF x
  return $ a |> Func p
fromTreeF (Inf x (getLocResolvedId -> o) y) = do
  (a, b) <- (,) <$> fromTreeF x <*> fromTreeF y
  return $ bFork [a, b] |> Func o
fromTreeF (Var (getLocResolvedId -> o) xs) = do
  xs' <- mapM fromTreeF xs
  return $ bFork xs' |> Func o
fromTreeF (Term (Just x)) = toBE x
fromTreeF (Term Nothing) = return pId

-- * Lookups

lookupFOpOnly :: LookupOp e => ULocName Rel -> BResult e (LocFixity F)
lookupFOpOnly o = do
  e <- ask
  case lookupOp e $ getLocated o of
    Nothing -> throwError $ OpNotFound o
    Just (Right (Fixity _ _ a)) ->
      throwError $ FlOpNotAllowed $ ResolvedOp o $ AN a
    Just (Left f) -> return $ withSameLoc o f

lookupOpOnly :: LookupOp e => ULocName Rel
                -> BResult e (Either FFixity FlFixity)
lookupOpOnly o = do
  e <- ask
  case lookupOp e $ getLocated o of
    Nothing -> throwError $ OpNotFound o
    Just f -> return f

-- * Primitive Symbols

synNameL :: String -> LocUnName f
synNameL = Loc Nothing . SN

base :: Monad m => String -> m ([BExpr] -> BExpr)
base = return . App . synNameL

baseA :: String -> [BExpr] -> BExpr
baseA = App . synNameL

baseF :: String -> BExpr
baseF = Func . synNameL

pId :: BExpr
pId = Func $ synNameL "_"

bnCompose, bnFork :: LocUnName Fl
bnCompose = synNameL "Compose"
bnFork = synNameL "Fork"

bFork :: [BExpr] -> BExpr
bFork = App bnFork

bC2 :: BExpr -> BExpr -> BExpr
bC2 a b = App bnCompose [a, b]

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
