module FP15.Compiler.CommaNotation where
import Control.Applicative
import Control.Monad.Writer
import Control.Monad.State

data CError o a = TwoSidedCommas Int Int o

data CommaInfix o a = CInfix [CommaNode o a]
                     deriving (Eq, Ord, Show, Read)
data CommaNode o a = ExprN !a | OpN !Int !o | CompN !(CommaInfix o a) !a
                     deriving (Eq, Ord, Show, Read)

data UncommaInfix o a = UInfix [UncommaNode o a]
data UncommaNode o a = UExpr !Int !a | UOp !o | UComp !(UncommaInfix o a) !a

data P3 o a = C3 !Int | O3 !o | N3 !a
data P4 o a = C4 !Int | O4 !o | OR4 !Int !o | N4 !a

getOffset :: CommaInfix o a -> Int
getOffset e = getSum $ snd $ runWriter (getExprOffset e)

getExprOffset :: CommaInfix o a -> Writer (Sum Int) ()
getExprOffset (CInfix ns) = mapM_ getNodeOffset ns

getNodeOffset :: CommaNode o a -> Writer (Sum Int) ()
getNodeOffset (CompN e _) = getExprOffset e
getNodeOffset (OpN m o) = tell (Sum m)
getNodeOffset _ = return ()

getOffsetsExpr :: CommaInfix o a -> [Int]
getOffsetsExpr (CInfix ns) = concatMap getOffsetsNode ns

getOffsetsNode :: CommaNode o a -> [Int]
getOffsetsNode (CompN e _) = getOffsetsExpr e
getOffsetsNode (OpN m _) = [m]
getOffsetsNode _ = []

getBase :: CommaInfix o a -> Int
getBase = negate . runningMin . getOffsetsExpr where
  runningMin = foldl min 0 . scanl (+) 0

walkCommaInfix :: CommaInfix o a -> State Int (UncommaInfix o a)
walkCommaInfix (CInfix ns) = UInfix <$> mapM walkCommaNode ns

walkCommaNode :: CommaNode o a -> State Int (UncommaNode o a)
walkCommaNode (ExprN a) = UExpr <$> get <*> return a
walkCommaNode (OpN m a) = modify (+ m) *> return (UOp a)
walkCommaNode (CompN n e) = UComp <$> walkCommaInfix n <*> return e

convCommaExpr :: CommaInfix o a -> UncommaInfix o a
convCommaExpr e = evalState (walkCommaInfix e) (getBase e)

toCommaInfix
  :: (c -> Either o a) -> [Either Int c] -> Either (CError o a) (CommaInfix o a)
toCommaInfix f = fmap CInfix . parse . map classify where
  classify (Left n) = C3 n
  classify (Right x)
    = case f x of
        Left o -> O3 o
        Right a -> N3 a

  parse = leftPass . rightPass
  rightPass :: [P3 o a] -> [P4 o a]
  leftPass :: [P4 o a] -> Either (CError o a) [CommaNode o a]

  rightPass (O3 o : C3 i : xs) = OR4 i o : rightPass xs
  rightPass (x : xs) = c34 x : rightPass xs
  rightPass [] = []

  c34 :: P3 o a -> P4 o a
  c34 (C3 i) = C4 i
  c34 (O3 o) = O4 o
  c34 (N3 x) = N4 x

  leftPass (C4 m : OR4 n a : _) = Left $ TwoSidedCommas m n a
  leftPass (C4 m : O4 o : xs) = (OpN (negate m) o :) <$> leftPass xs
  leftPass [] = return []
  leftPass _ = undefined
