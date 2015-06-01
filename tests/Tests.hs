module Main where
import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

main :: IO ()
main = defaultMainWithOpts [ testCase "t1" t1 ] mempty

t1 :: Assertion
t1 = [1, 2] @?= [1, 2]
