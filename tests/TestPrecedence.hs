module TestPrecedence where
import Control.Applicative
import Test.QuickCheck
import FP15.Compiler.Precedence

-- | A list of 'PrecNode' that satistify the precondition of precedence parsing.
newtype PrecNodeList o a = PrecNodeList [PrecNode o a]
                         deriving (Eq, Ord, Show, Read)

-- TODO multiple variadic ops is causing an error
instance (Arbitrary o, Arbitrary a) => Arbitrary (PrecNodeList o a) where
  arbitrary = PrecNodeList <$> ((++) <$> oneof [pure [], (:[]) <$> nonTermNode]
                                     <*> sized genPrecNodeListOfSize)

instance Arbitrary Assoc where
  arbitrary = elements [VarA, LeftA, RightA]

instance (Arbitrary o, Arbitrary a) => Arbitrary (PrecNode o a) where
  arbitrary = oneof [ nonTermNode, termNode ]

instance (Arbitrary o, Arbitrary a) => Arbitrary (Tree o a) where
  arbitrary = sized genTreeOfSize

genPrecNodeListOfSize :: (Arbitrary o, Arbitrary a) => Int -> Gen [PrecNode o a]
genPrecNodeListOfSize n
  | n <= 0 = (:[]) <$> termNode
  | otherwise = do a <- genTermPart
                   b <- genInfPart
                   xs <- genPrecNodeListOfSize (n - 2)
                   return $ a ++ b : xs

genTermPart :: (Arbitrary o, Arbitrary a) => Gen [PrecNode o a]
genTermPart = do
  pre <- resize 3 $ listOf (PreN <$> arbitrary <*> arbitrary)
  t <- termNode
  return $ pre ++ [t]

genInfPart :: (Arbitrary o, Arbitrary a) => Gen (PrecNode o a)
genInfPart = InfN <$> oneof [pure LeftA, pure RightA] <*> arbitrary <*> arbitrary

nonTermNode :: (Arbitrary o, Arbitrary a) => Gen (PrecNode o a)
nonTermNode = oneof [ PreN <$> arbitrary <*> arbitrary
                    , InfN <$> arbitrary <*> arbitrary <*> arbitrary ]
termNode :: (Arbitrary o, Arbitrary a) => Gen (PrecNode o a)
termNode = TermN <$> arbitrary

genTreeOfSize :: (Arbitrary o, Arbitrary a) => Int -> Gen (Tree o a)
genTreeOfSize m
  | m <= 1 = Term <$> arbitrary
  | otherwise = oneof [ Pre <$> arbitrary <*> genTreeOfSize (m - 1)
                      , Inf <$> genTreeOfSize (m `div` 2)
                            <*> arbitrary <*> genTreeOfSize (m `div` 2)
                      , Var <$> arbitrary <*> resize 8 (genTreeListOfSize m)
                      , Term <$> arbitrary ]

genTreeListOfSize :: (Arbitrary o, Arbitrary a) => Int -> Gen [Tree o a]
genTreeListOfSize m = do n <- oneof $ map pure [1..8]
                         vectorOf n (genTreeOfSize $ 1 + m `div` n)

test x = quickCheckWith stdArgs { maxSuccess = 2500 } x
test1 = test prop_parsePrecInvariantNoError
test2 = test prop_parsePrecNoError

prop_parsePrecInvariantNoError = (\(PrecNodeList x) -> seq (unwrap $ parsePrec (x :: [PrecNode (Maybe Bool) (Maybe Bool)])) True)
prop_parsePrecNoError = (\x -> seq (parsePrec (x :: [PrecNode (Maybe Bool) (Maybe Bool)])) True)

unwrap :: Show e => Either e b -> b
unwrap (Left x) = error $ show x
unwrap (Right x) = x

