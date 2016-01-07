{-# LANGUAGE FlexibleContexts #-}
module FP15.Evaluator.Error where
import Control.Monad.Error
import FP15.Disp
import FP15.Name
import FP15.Evaluator.Types

runtimeError :: MonadError RuntimeError m => (StackTrace -> RuntimeError) -> m a
pushStackTrace :: StackFrame -> RuntimeError -> RuntimeError
markStackFrame :: MonadError RuntimeError m => StackFrame -> m a -> m a
markFunc :: MonadError RuntimeError m => Located String -> m a -> m a

runtimeError e = throwError $ e emptyStackTrace

raiseContractViolation :: MonadError RuntimeError m => Contract b -> FPValue -> m a
raisePassMismatchError :: MonadError RuntimeError m => Int -> Int -> m a
raiseErrorMessage :: MonadError RuntimeError m => String -> m a
raiseEnvAccessError :: MonadError RuntimeError m => Int -> m a

raiseContractViolation c v = runtimeError $ ContractViolation (show c) (disp v)
raisePassMismatchError m n = runtimeError $ PassMismatchError m n
raiseErrorMessage = runtimeError . ErrorMessage
raiseEnvAccessError = runtimeError . EnvAccessError

pushStackTrace s (ContractViolation c v (StackTrace st)) =
  ContractViolation c v $ StackTrace (s:st)
pushStackTrace s (PassMismatchError m n (StackTrace st)) =
  PassMismatchError m n $ StackTrace (s:st)
pushStackTrace s (ErrorMessage m (StackTrace st)) = ErrorMessage m $ StackTrace st
pushStackTrace s (EnvAccessError e (StackTrace st)) = ErrorMessage "EnvAccessError" $ StackTrace st

markStackFrame s = (`catchError` rethrowWithExtendedStackTrace)
  where rethrowWithExtendedStackTrace = throwError . pushStackTrace s

markFunc f = markStackFrame $ StackFrame (Just f)
