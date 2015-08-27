module FP15.Compiler.CommaNotation where
import Control.Applicative
import Control.Monad.Writer
import Control.Monad.State

data CommaInfix o a = CInfix [CommaNode o a]
                     deriving (Eq, Ord, Show, Read)
data CommaNode o a = ExprN a | OpN Int o | CompN (CommaInfix o a) a
                     deriving (Eq, Ord, Show, Read)

data UncommaInfix o a = UInfix [UncommaNode o a]
data UncommaNode o a = UExpr Int a | UOp o | UComp (UncommaInfix o a) a

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
