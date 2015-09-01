{-# LANGUAGE Safe, GADTs, RankNTypes, ExistentialQuantification #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE FlexibleInstances #-}
module FP15.Evaluator.RuntimeError where
import Control.Monad.Error
import Text.PrettyPrint
import FP15.Disp
import FP15.Name
import FP15.Evaluator.FPValue
import FP15.Evaluator.ContractType

-- * Error and Diagnostics

data StackFrame = StackFrame (Maybe (Located String))
                deriving (Eq, Ord, Show, Read)
newtype StackTrace = StackTrace [StackFrame]
                   deriving (Eq, Ord, Show, Read)

emptyStackTrace :: StackTrace
emptyStackTrace = StackTrace []

-- | An FP15 runtime error.
data RuntimeError = forall a. ContractViolation { contractViolated :: Contract a
                                                , offendingValue :: FPValue
                                                , stackTrace :: StackTrace }
                  | PassMismatchError { expectedLength :: Int
                                      , actualLength :: Int
                                      , stackTrace :: StackTrace }
                  | ErrorMessage { messageText :: String
                                 , stackTrace :: StackTrace }

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

joinLines :: [Doc] -> Doc
joinLines = vcat


