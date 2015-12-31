module FP15.Evaluator.FPRef where
import FP15.Evaluator.FP
import FP15.Evaluator.FPValue

newFPRef :: a -> FP (FPRef a)
readFPRef :: FPRef a -> FP a
writeFPRef :: FPRef a -> FP ()
modifyFPRef :: FPRef a -> (a -> a) -> FP ()

ni = error "FPRef: not implemented"

newFPRef x = return ni
readFPRef r = return ni
writeFPRef r = return ni
modifyFPRef r f = return ni
