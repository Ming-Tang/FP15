module FP15.Compiler.CommaNotation (
  CError(..)
, CommaInfix(..)
, CommaNode(..)
, UncommaInfix(..)
, UncommaNode(..)

, GetSecondLevel
, OpClassifier
, NodeClassifier

, getOffset
, getBase
, convCommaExpr

, toCommaInfix
) where
import Control.Applicative
import Control.Monad.Writer
import Control.Monad.State
import Data.List.Split
import FP15.Types()

data CError o a = TwoSidedCommas Int Int o
                deriving (Eq, Ord, Show, Read)

data CommaInfix o a = CInfix [CommaNode o a]
                   deriving (Eq, Ord, Show, Read)
data CommaNode o a = CompN0 ![a]
                   | CompN1 !(CommaInfix o a) ![a]
                   | OpN !Int !o
                   deriving (Eq, Ord, Show, Read)

data UncommaInfix o a = UInfix [UncommaNode o a]
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
walkCommaInfix (CInfix ns) = UInfix <$> mapM walkCommaNode ns

walkCommaNode :: CommaNode o a -> State Int (UncommaNode o a)
walkCommaNode (CompN0 a) = UComp0 <$> get <*> return a
walkCommaNode (OpN m a) = do
  n <- get
  modify (+ m)
  n' <- get
  return $ UOp (n, n') a

walkCommaNode (CompN1 n e) = UComp1 <$> walkCommaInfix n <*> return e

convCommaExpr :: CommaInfix o a -> UncommaInfix o a
convCommaExpr e = evalState (walkCommaInfix e) (getBase e)
-- TODO doesn't handle missing operands properly

toCommaInfix
  :: NodeClassifier o a c
      -> [Either Int c]
      -> Either (CError o a) (CommaInfix o a)
toCommaInfix p = fmap CInfix . parse p . map (classify p)

classify :: NodeClassifier o a c -> Either Int c -> P3 o a
classify _ (Left n) = C3 n
classify (_, f) (Right x)
  = case f x of
      Left o -> O3 o
      Right a -> N3 a

collectCommas :: [P3 o a] -> Either (CError o a) [P4 o a]
parse :: NodeClassifier o a c -> [P3 o a] -> Either (CError o a) [CommaNode o a]

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
