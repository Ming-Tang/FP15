module FP15.Evaluator.Error where
import Control.Monad.Error
import FP15.Value
import FP15.Name
import FP15.Evaluator.Types

runtimeError :: (StackTrace -> RuntimeError) -> ResultOf a
pushStackTrace :: StackFrame -> RuntimeError -> RuntimeError
markStackFrame :: StackFrame -> ResultOf a -> ResultOf a
markFunc :: Located String -> ResultOf a -> ResultOf a

runtimeError e = throwError $ e emptyStackTrace

raiseContractViolation :: Contract b -> Value -> ResultOf a
raisePassMismatchError :: Int -> Int -> ResultOf a
raiseErrorMessage :: String -> ResultOf a

raiseContractViolation c v = runtimeError $ ContractViolation c v
raisePassMismatchError m n = runtimeError $ PassMismatchError m n
raiseErrorMessage = runtimeError . ErrorMessage

pushStackTrace s (ContractViolation c v (StackTrace st)) =
  ContractViolation c v $ StackTrace (s:st)
pushStackTrace s (PassMismatchError m n (StackTrace st)) =
  PassMismatchError m n $ StackTrace (s:st)
pushStackTrace s (ErrorMessage m (StackTrace st)) = ErrorMessage m $ StackTrace st

markStackFrame s = (`catchError` rethrowWithExtendedStackTrace)
  where rethrowWithExtendedStackTrace = throwError . pushStackTrace s

markFunc f = markStackFrame $ StackFrame (Just f)
