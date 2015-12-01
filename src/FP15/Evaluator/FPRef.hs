module FP15.Evaluator.FPRef where
import Data.IORef
import FP15.Evaluator.FP

type Rev = (Int, Int)
data FPRef a = FPRef Rev (IORef a)

newFPRef :: a -> FP (FPRef a)
readFPRef :: FPRef a -> FP a
writeFPRef :: FPRef a -> FP ()
modifyFPRef :: FPRef a -> (a -> a) -> FP ()
modifyFPRef' :: FPRef a -> (a -> a) -> FP ()

