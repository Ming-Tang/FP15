module TestReduction where
import Control.Applicative
import Control.Monad.Trans.Reader
import FP15.Types
import FP15.Compiler.Types
import FP15.Compiler.Precedence
import FP15.Compiler.Reduction
import Data.Map.Strict as M
import ArbitraryInstances
import FP15.Compiler.Reduction.BExpr

--type LOP = M.Map (Name Unknown) (Either FFixity FlFixity)
type LOP = EmptyLookup
prop_doSmartSplit_idompotent lop ast =
  runReaderT (doSmartSplit ast) (lop :: LOP) == runReaderT (doSmartSplit ast >>= doSmartSplit) lop

