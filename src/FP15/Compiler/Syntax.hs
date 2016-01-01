-- |
-- The 'FP15.Compiler.Syntax' module contains FP15-specific parsing logic for
-- syntactic sugars.
module FP15.Compiler.Syntax (
  module FP15.Compiler.Syntax.SmartSplit
, module FP15.Compiler.Syntax.Precedence
, module FP15.Compiler.Syntax.CommaNotation
) where
import FP15.Compiler.Syntax.SmartSplit
import FP15.Compiler.Syntax.Precedence
import FP15.Compiler.Syntax.CommaNotation

{-# ANN module "HLint: ignore Use import/export shortcut" #-}
