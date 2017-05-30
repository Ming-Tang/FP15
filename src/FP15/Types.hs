{-# LANGUAGE Trustworthy #-}
module FP15.Types (
  module FP15.Xtn
, module FP15.Disp
, module FP15.Name
, module FP15.Value
, module FP15.Expr
, module FP15.Modules
, module FP15.Types
, Void
, Map
) where
import Data.Map.Strict(Map)
import qualified Data.List.NonEmpty as NE
import Data.Void(Void)
import FP15.Value
import FP15.Xtn
import FP15.Disp
import FP15.Name
import FP15.Expr
import FP15.Modules

-- * Type Synonyms
-- Short identifiers are needed because of 80-char line length limit.

-- | A strict set.
type Set e = Map e ()

-- | A non-empty list.
type NE a = NE.NonEmpty a

{-# ANN module "HLint: ignore Use import/export shortcut" #-}
