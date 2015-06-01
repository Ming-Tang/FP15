module FP15.Compiler.Errors where
import FP15.Types

data ImportError = CannotImport ModuleName
                 | NameNotInModule String
                 deriving (Eq, Ord, Show, Read)

