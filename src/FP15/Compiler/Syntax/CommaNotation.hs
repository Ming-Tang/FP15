module FP15.Compiler.Syntax.CommaNotation (
  CError(..)
, CommaInfix(..)
, CommaNode(..)
, UncommaInfix(..)
, UncommaNode(..)

, GetSecondLevel
, OpClassifier
, OpKindClassifier
, NodeClassifier

, getOffset
, getBase
, convCommaExpr

, toCommaInfix
, insertIndexers
) where
import Control.Applicative
import Control.Monad.Writer
import Control.Monad.State
import Data.Maybe(isJust)
import Data.List(partition)
import Data.List.Split
import FP15.Types()

data CError o a = TwoSidedCommas Int Int o
                deriving (Eq, Ord, Show, Read)

-- | Invariant: No consecutive 'CompN0'/'CompN1's
data CommaInfix o a = CInfix ![CommaNode o a] -- TODO add offset
                   deriving (Eq, Ord, Show, Read)
data CommaNode o a = CompN0 ![a]
                   | CompN1 !(CommaInfix o a) ![a]
                   | OpN !Int !o
                   deriving (Eq, Ord, Show, Read)

-- | Invariant: No consecutive 'UComp0'/'UComp1's
data UncommaInfix o a = UInfix !Int ![UncommaNode o a]
                     deriving (Eq, Ord, Show, Read)
data UncommaNode o a = UComp0 !Int ![a]
                     | UComp1 !(UncommaInfix o a) ![a]
                     | UOp !(Int, Int) !o
                     deriving (Eq, Ord, Show, Read)

data P3 o a = C3 !Int | O3 !o | N3 !a
data P4 o a = C4 !Int | O4 !o | OR4 !Int !o | N4 !a
data C o a = CO !Int !o | CC0 [a] | CC1 [P3 o a] [a]

type GetSecondLevel a c = a -> Maybe [Either Int c]
type OpClassifier o a c = c -> Either o a
type NodeClassifier o a c = (GetSecondLevel a c, OpClassifier o a c)
type OpKindClassifier o = o -> Bool

getOffset :: CommaInfix o a -> Int
getOffset e = getSum $ snd $ runWriter (getExprOffset e)

getExprOffset :: CommaInfix o a -> Writer (Sum Int) ()
getExprOffset (CInfix ns) = mapM_ getNodeOffset ns

getNodeOffset :: CommaNode o a -> Writer (Sum Int) ()
getNodeOffset (CompN1 e _) = getExprOffset e
getNodeOffset (OpN m o) = tell (Sum m)
getNodeOffset _ = return ()

getOffsetsExpr :: CommaInfix o a -> [Int]
getOffsetsExpr (CInfix ns) = concatMap getOffsetsNode ns

getOffsetsNode :: CommaNode o a -> [Int]
getOffsetsNode (CompN1 e _) = getOffsetsExpr e
getOffsetsNode (OpN m _) = [m]
getOffsetsNode _ = []

getBase :: CommaInfix o a -> Int
getBase = negate . runningMin . getOffsetsExpr where
  runningMin = foldl min 0 . scanl (+) 0

walkCommaInfix :: CommaInfix o a -> State Int (UncommaInfix o a)
walkCommaInfix (CInfix ns) = UInfix <$> get <*> mapM walkCommaNode ns

walkCommaNode :: CommaNode o a -> State Int (UncommaNode o a)
walkCommaNode (CompN0 a) = UComp0 <$> get <*> return a
walkCommaNode (OpN m a) = do
  n <- get
  modify (+ m)
  n' <- get
  return $ UOp (n, n') a

walkCommaNode (CompN1 n e) = UComp1 <$> walkCommaInfix n <*> return e

-- TODO take offset as argument here
convCommaExpr :: (Int, CommaInfix o a) -> UncommaInfix o a
convCommaExpr (m0, e) = evalState (walkCommaInfix e) (getBase e + m0)

toCommaInfix
  :: NodeClassifier o a c
      -> [Either Int c]
      -> Either (CError o a) (Int, CommaInfix o a)
toCommaInfix p (Left m0:xs) = toCommaInfix' p xs m0
toCommaInfix p xs = toCommaInfix' p xs 0

toCommaInfix'
  :: NodeClassifier o a c -> [Either Int c] -> t
     -> Either (CError o a) (t, CommaInfix o a)
toCommaInfix' p xs m0
  = fmap (\x -> (m0, CInfix x)) . parse p . map (classify p) $ xs

classify :: NodeClassifier o a c -> Either Int c -> P3 o a
classify _ (Left n) = C3 n
classify (_, f) (Right x)
  = case f x of
      Left o -> O3 o
      Right a -> N3 a

collectCommas :: [P3 o a] -> Either (CError o a) [P4 o a]
parse
  :: NodeClassifier o a c -> [P3 o a] -> Either (CError o a) [CommaNode o a]

collectCommas = leftPass . rightPass
parse p = joinPass p <=< collectCommas

rightPass :: [P3 o a] -> [P4 o a]
leftPass :: [P4 o a] -> Either (CError o a) [P4 o a]
joinPass :: NodeClassifier o a c -> [P4 o a] -> Either (CError o a) [CommaNode o a]

rightPass (O3 o : C3 i : xs) = OR4 i o : rightPass xs
rightPass (x : xs) = c34 x : rightPass xs
rightPass [] = []

c34 :: P3 o a -> P4 o a
c34 (C3 i) = C4 i
c34 (O3 o) = O4 o
c34 (N3 x) = N4 x

leftPass (C4 m : OR4 n a : _) = Left $ TwoSidedCommas m n a
leftPass (C4 m : O4 o : xs) = (OR4 (negate m) o :) <$> leftPass xs
leftPass (x : xs) = (x :) <$> leftPass xs
leftPass [] = return []

joinPass p = mapM (process p . classifyGrp p) . filter (not . null)
                                              . split (whenElt isO) . filter isNotC4

isNotC4, isO :: P4 o a -> Bool

isNotC4 (C4 _) = False
isNotC4 _ = True

isO (O4 _) = True
isO (OR4 _ _) = True
isO _ = False

isUOp :: UncommaNode o a -> Bool
isUOp (UOp _ _) = True
isUOp _ = False

classifyGrp :: NodeClassifier o a c -> [P4 o a] -> C o a
classifyGrp _ [O4 o] = CO 0 o
classifyGrp _ [OR4 m o] = CO m o
classifyGrp p@(g, f) xs0@((fromN4 -> x):(map fromN4 -> xs))
  | all (not . isO) xs0 =
    case g x of
      Nothing -> CC0 (x:xs)
      Just ys -> CC1 (map (classify p) ys) xs
  | otherwise = error "classifyGrp: impossible (there are non-Os)"
classifyGrp _ [] = error "classifyGrp: impossible (empty)"

process :: NodeClassifier o a c -> C o a -> Either (CError o a) (CommaNode o a)
process _ (CO m o) = return $ OpN m o
process _ (CC0 xs) = return $ CompN0 xs
process p (CC1 t xs) = (flip CompN1 xs . CInfix) <$> parse p t

fromN4 :: P4 o a -> a
fromN4 (N4 x) = x
fromN4 _ = error "toCommaInfix: fromN4: Not N4."

-- | The 'insertIndexers' function inserts indexers in the operands. This
-- function requires the knowledge if an operator is prefix or infix, but
-- precedence and associativity are not required.
insertIndexers
  :: OpKindClassifier o -- ^ Determines if an operator is prefix or infix.
     -> (Int -> a) -- ^ Generates an indexer.
     -> UncommaInfix o a -> UncommaInfix o a
insertIndexers isI indexer (UInfix m0 xs0)
  = UInfix m0 $ evalState walker m0 where
  walker = walk isI indexer $ map cls $ split (whenElt isInfix) xs0

  tryInfix (UOp c o) | isI o = Just (c, o)
  tryInfix _ = Nothing

  isInfix = isJust . tryInfix

  cls [tryInfix -> Just inf] = Left inf
  -- for operands: partition into (prefix, begin operand)
  cls ys | all (not . isInfix) ys = Right $ partition isUOp ys
         | otherwise = error "insertIndexers: impossible (split didn't work)"

walk
  :: OpKindClassifier o -> (Int -> a)
     -> [Either ((Int, Int), o) ([UncommaNode o a], [UncommaNode o a])]
     -> State Int [UncommaNode o a]
walk _ _ [] = return []
walk isI indexer (Left ((m', n), o) : xs) = do
  m <- get
  -- XXX validation logic doesn't work with inner exprs involved.
  {-if m /= m' then
    error $ "insertIndexers: walk: mismatch: " ++ show (m, m')
  else do-}
  put n
  (UOp (m', n) o :) <$> walk isI indexer xs

walk isI indexer (Right (l, r) : xs) = do
  m <- get
  let pd = case r of
         [] -> [UComp0 m [indexer m]]
         UComp1 e e1 : ys -> UComp1 (insertIndexers isI indexer e) e1 : ys
         UComp0 k es : ys ->
          if k /= m then error ("walk: UComp1 mismatch: " ++ show (m, k))
          else UComp0 k (indexer m : es) : ys -- TODO validate k?
         _ -> error ("walk: " ++ show (length r) ++ " consecutive operands.")
  ((l ++ pd) ++) <$> walk isI indexer xs
