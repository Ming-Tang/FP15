module FP15.Evaluator.FPRef where
import Data.IORef

type Rev = (Int, Int)
data FPRef a = FPRef Rev (IORef a)
