{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ArbitraryCode where
import Control.Applicative hiding (Const)
import Test.QuickCheck
import FP15.Types hiding (NE, Map)
import FP15.Compiler.Types
import FP15.Value
import Data.Map(Map, fromList)


