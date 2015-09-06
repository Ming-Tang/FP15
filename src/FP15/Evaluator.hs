-- |
-- The @FP15.Evaluator@ module contains the Haskell implementation of FP15.
-- The evaluator accepts FP15 definitions in the form of 'BaseExpr', which is generated
-- by the compiler, and translates the definitions into a map of Haskell functions.
module FP15.Evaluator where
import FP15.Evaluator.Types()
import FP15.Evaluator.Contract()
import FP15.Evaluator.Number()
import FP15.Evaluator.Standard()
import FP15.Evaluator.Translation()

