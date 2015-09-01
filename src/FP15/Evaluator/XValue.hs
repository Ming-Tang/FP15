module FP15.Evaluator.XValue (
  module FP15.Value
, XValue(..)
) where
import FP15.Value
import FP15.Evaluator.FPRef

data XValue = Value Value
            | Lambda (XValue -> XValue)
            | Ref (FPRef XValue)
            | RealWorld

