{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveGeneric #-}
module FP15.Evaluator.Types (
  module FP15.Evaluator.Types
, module FP15.Evaluator.RuntimeError
, module FP15.Evaluator.FPRef
, module FP15.Evaluator.ContractType
, module FP15.Evaluator.FPValue
, module FP15.Evaluator.FP
) where
import GHC.Generics
import Control.DeepSeq
import FP15.Name
import FP15.Value
import FP15.Evaluator.RuntimeError
import FP15.Evaluator.FPRef
import FP15.Evaluator.FPValue
import FP15.Evaluator.ContractType
import FP15.Evaluator.FP(FP(..))

-- * Expression

type Ident = String
data BaseExpr = Const Value
              | Func (Located Ident)
              | Compose [BaseExpr]
              | If BaseExpr BaseExpr BaseExpr
              | Fork [BaseExpr]
              | Hook [BaseExpr]
              | Map BaseExpr
              | Filter BaseExpr
              | While BaseExpr BaseExpr
              | Mark Ident BaseExpr
              | Get Int
              | With BaseExpr
              deriving (Eq, Show, Read, Generic)

instance NFData BaseExpr
type FPResult = FP FPValue

-- | An FP15 function, which takes a 'Value' and returns a 'Value' or a
-- 'RuntimeError'.
type FPFunc = FPValue -> FPResult

{-# ANN module "HLint: ignore Use import/export shortcut" #-}
