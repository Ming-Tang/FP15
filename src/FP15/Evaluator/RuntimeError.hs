{-# LANGUAGE Safe, GADTs, RankNTypes, ExistentialQuantification #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE FlexibleInstances #-}
module FP15.Evaluator.RuntimeError where
import Control.Monad.Error
import Text.PrettyPrint
import FP15.Disp
import FP15.Name

-- * Error and Diagnostics

data StackFrame = StackFrame (Maybe (Located String))
                deriving (Eq, Ord, Show, Read)
newtype StackTrace = StackTrace [StackFrame]
                   deriving (Eq, Ord, Show, Read)

emptyStackTrace :: StackTrace
emptyStackTrace = StackTrace []

-- | An FP15 runtime error.
data RuntimeError = ContractViolation { contractViolated :: String
                                      , offendingValue :: String
                                      , stackTrace :: StackTrace }
                  | PassMismatchError { expectedLength :: Int
                                      , actualLength :: Int
                                      , stackTrace :: StackTrace }
                  | ErrorMessage { messageText :: String
                                 , stackTrace :: StackTrace }
                  | EnvAccessError { offendingIndex :: Int }

instance Error RuntimeError where
  strMsg s = ErrorMessage s emptyStackTrace
  noMsg = ErrorMessage "Runtime error." emptyStackTrace

instance Disp StackTrace where
  pretty (StackTrace st) =
    joinLines $ text "Stack Trace:" : map pretty (reverse st)

instance Disp StackFrame where
  pretty (StackFrame func) =
    nest 2 $ maybe (text "<func unknown>") pretty func

instance Disp RuntimeError where
  pretty (ContractViolation c v st) =
    joinLines [text "Contract Violation:",
               text "Contract: " <> text (show c),
               text "Value: " <> text (disp v),
               pretty st]

  pretty (PassMismatchError m n st) =
    joinLines [text ("Pass: Arity mismatch: Expecting " ++ show m ++ " args"),
               text ("but got " ++ show n ++ "."),
               text (show st)]

  pretty (ErrorMessage s st) =
    joinLines [text "Error:" <+> text s, text (show st)]

  pretty (EnvAccessError i) =
    text "Env access error: " <+> text (show i)

joinLines :: [Doc] -> Doc
joinLines = vcat
