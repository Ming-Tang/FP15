{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ArbitraryInstances where
import Control.Applicative hiding (Const)
import Test.QuickCheck
import FP15.Types hiding (NE, Map)
import FP15.Compiler.Types
import FP15.Value
import Data.Map(Map, fromList)
import Data.List.NonEmpty(NonEmpty(..))
import ArbitraryTokens
import Main()

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Map k v) where
  arbitrary = fromList <$> arbitrary

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = (:|) <$> arbitrary <*> arbitrary

exprASTOfSize :: Int -> Gen ExprAST
exprASTOfSize n = oneof (zs ++ (if n <= 1 then [] else ps)) where
  zs = [ TValue <$> resize n arbitrary
       , TFunc <$> resize (min n 10) arbitrary
       , TOperator <$> resize (min n 10) arbitrary
       , TDotOperator <$> resize (min n 10) arbitrary
       , TIndex <$> (getPositive <$> arbitrary) ]
  ps = [ TApp <$> resize (min n 10) arbitrary <*> arbList
       , TIf <$> arb3 <*> arb3 <*> arb3
       , TFork <$> arbList
       , THook <$> arbList
       , TUnresolvedPrimaryList <$> arbList
       , TUnresolvedInfixNotation <$> arbList ]
       -- TODO TLet
  arb3 = exprASTOfSize (n `div` 3)
  arbList = do m <- max 0 . min n <$> arbitrary
               vectorOf m (exprASTOfSize $ n `div` m)


valueOfSize :: Int -> Gen Value
valueOfSize n = oneof (zs ++ (if n <= 1 then [] else [List <$> arbList])) where
  zs = [ Bool <$> arbitrary, Char <$> arbitrary, Int <$> arbitrary
       , Real <$> arbitrary, Symbol <$> resize n arbitrary
       , String <$> resize n arbitrary ]
  arbList = do m <- max 0 . min n <$> arbitrary
               vectorOf m (valueOfSize $ n `div` m)

instance Arbitrary ExprAST where
  arbitrary = sized exprASTOfSize

instance Arbitrary Value where
  arbitrary = sized valueOfSize

instance Arbitrary (Id Unknown) where
   arbitrary = Id <$> arbOpName

instance Arbitrary (Id FOp) where
   arbitrary = Id <$> arbOpName

instance Arbitrary (Id FlOp) where
   arbitrary = Id <$> arbOpName

instance Arbitrary (Id F) where
   arbitrary = Id <$> arbFName

instance Arbitrary (Id Fl) where
   arbitrary = Id <$> arbFlName

instance Arbitrary ModuleName where
   arbitrary = M <$> listOf arbModName

instance Arbitrary (Name Unknown) where
    arbitrary = N <$> listOf arbModName <*> arbOpName

instance Arbitrary (Name FOp) where
    arbitrary = N <$> listOf arbModName <*> arbOpName

instance Arbitrary (Name FlOp) where
    arbitrary = N <$> listOf arbModName <*> arbOpName

instance Arbitrary (Name F) where
    arbitrary = N <$> listOf arbModName <*> arbFName

instance Arbitrary (Name Fl) where
    arbitrary = N <$> listOf arbModName <*> arbFlName

instance Arbitrary (Fixity Unknown) where
   arbitrary = Fixity <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (Fixity F) where
   arbitrary = Fixity <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (Fixity Fl) where
   arbitrary = Fixity <$> arbitrary <*> arbitrary <*> arbitrary

{- -------------------------------------- -}


instance Arbitrary EmptyLookup where
        arbitrary = return EmptyLookup


instance (Arbitrary a, Arbitrary b) => Arbitrary
         (FallbackLookup a b) where
        arbitrary
          = do x1 <- arbitrary
               x2 <- arbitrary
               return (Fallback x1 x2)


instance (Arbitrary f, Arbitrary fl, Arbitrary ffx, Arbitrary flfx) =>
         Arbitrary (ModuleBody Name f fl ffx flfx) where
        arbitrary
          = do x1 <- arbitrary
               x2 <- arbitrary
               x3 <- arbitrary
               x4 <- arbitrary
               return (Module x1 x2 x3 x4)


instance (Arbitrary f, Arbitrary fl, Arbitrary ffx, Arbitrary flfx) =>
         Arbitrary (ModuleBody Id f fl ffx flfx) where
        arbitrary
          = do x1 <- arbitrary
               x2 <- arbitrary
               x3 <- arbitrary
               x4 <- arbitrary
               return (Module x1 x2 x3 x4)


instance Arbitrary ModuleInterface where
        arbitrary
          = do x1 <- arbitrary
               return (ModuleInterface x1)


instance Arbitrary MIContext where
        arbitrary
          = do x1 <- arbitrary
               x2 <- arbitrary
               return (MIContext x1 x2)


instance Arbitrary SourceMapping where
        arbitrary
          = do x1 <- arbitrary
               return (SourceMapping x1)


instance Arbitrary ImportedNames where
        arbitrary
          = do x1 <- arbitrary
               return (Imported x1)


instance Arbitrary StaticState where
        arbitrary
          = do x1 <- arbitrary
               x2 <- arbitrary
               x3 <- arbitrary
               x4 <- arbitrary
               x5 <- arbitrary
               return (SS x1 x2 x3 x4 x5)


instance Arbitrary ReducingModule where
        arbitrary
          = do x1 <- arbitrary
               return (Reducing x1)


instance Arbitrary ReducingTag where
        arbitrary
          = do x <- choose (0 :: Int, 2)
               case x of
                   0 -> return Normal
                   1 -> do x1 <- arbitrary
                           return (Blocked x1)
                   2 -> return Finished
                   _ -> error "FATAL ERROR: Arbitrary instance, logic bug"


instance Arbitrary ReducingModuleState where
        arbitrary
          = do x1 <- arbitrary
               x2 <- arbitrary
               x3 <- arbitrary
               return (ReducingModuleState x1 x2 x3)


instance Arbitrary ExprState where
        arbitrary
          = do x <- choose (0 :: Int, 3)
               case x of
                   0 -> do x1 <- arbitrary
                           return (Unresolved x1)
                   1 -> do x1 <- arbitrary
                           return (Unlifted x1)
                   2 -> do x1 <- arbitrary
                           return (Unreduced x1)
                   3 -> do x1 <- arbitrary
                           return (Reduced x1)
                   _ -> error "FATAL ERROR: Arbitrary instance, logic bug"


instance Arbitrary CompiledModule where
        arbitrary
          = do x1 <- arbitrary
               return (Compiled x1)


instance Arbitrary CompiledModuleItem where
        arbitrary
          = do x1 <- arbitrary
               x2 <- arbitrary
               x3 <- arbitrary
               return (CompiledModuleItem x1 x2 x3)


instance Arbitrary CompiledModuleSet where
        arbitrary
          = do x1 <- arbitrary
               return (CompiledModuleSet x1)


instance Arbitrary n => Arbitrary (Bound n) where
        arbitrary
          = do x <- choose (0 :: Int, 2)
               case x of
                   0 -> do x1 <- arbitrary
                           return (UB x1)
                   1 -> do x1 <- arbitrary
                           x2 <- arbitrary
                           return (BS x1 x2)
                   2 -> do x1 <- arbitrary
                           x2 <- arbitrary
                           return (BV x1 x2)
                   _ -> error "FATAL ERROR: Arbitrary instance, logic bug"


instance Arbitrary SrcPos where
        arbitrary
          = do x1 <- arbitrary
               x2 <- arbitrary
               x3 <- arbitrary
               x4 <- arbitrary
               return (SrcPos x1 x2 x3 x4)


instance Arbitrary a => Arbitrary (Located a) where
        arbitrary
          = do x1 <- arbitrary
               x2 <- arbitrary
               return (Loc x1 x2)


instance Arbitrary F where
        arbitrary
          = do x <- choose (0 :: Int, -1)
               case x of
                   _ -> error "FATAL ERROR: Arbitrary instance, logic bug"


instance Arbitrary Fl where
        arbitrary
          = do x <- choose (0 :: Int, -1)
               case x of
                   _ -> error "FATAL ERROR: Arbitrary instance, logic bug"


instance Arbitrary FOp where
        arbitrary
          = do x <- choose (0 :: Int, -1)
               case x of
                   _ -> error "FATAL ERROR: Arbitrary instance, logic bug"


instance Arbitrary FlOp where
        arbitrary
          = do x <- choose (0 :: Int, -1)
               case x of
                   _ -> error "FATAL ERROR: Arbitrary instance, logic bug"


instance Arbitrary Unknown where
        arbitrary
          = do x <- choose (0 :: Int, -1)
               case x of
                   _ -> error "FATAL ERROR: Arbitrary instance, logic bug"


instance Arbitrary Expr where
        arbitrary
          = do x <- choose (0 :: Int, 2)
               case x of
                   0 -> do x1 <- arbitrary
                           return (Const x1)
                   1 -> do x1 <- arbitrary
                           x2 <- arbitrary
                           return (App x1 x2)
                   2 -> do x1 <- arbitrary
                           return (Func x1)
                   _ -> error "FATAL ERROR: Arbitrary instance, logic bug"


instance Arbitrary BExpr where
        arbitrary
          = do x <- choose (0 :: Int, 3)
               case x of
                   0 -> do x1 <- arbitrary
                           return (BConst x1)
                   1 -> do x1 <- arbitrary
                           x2 <- arbitrary
                           return (BApp x1 x2)
                   2 -> do x1 <- arbitrary
                           return (BFunc x1)
                   3 -> do x1 <- arbitrary
                           x2 <- arbitrary
                           return (BLet x1 x2)
                   _ -> error "FATAL ERROR: Arbitrary instance, logic bug"


instance Arbitrary ModuleSource where
        arbitrary
          = do x1 <- arbitrary
               x2 <- arbitrary
               return (ModuleSource x1 x2)


instance Arbitrary ModuleResolutionError where
        arbitrary
          = do x <- choose (0 :: Int, 1)
               case x of
                   0 -> return ModuleNotFound
                   1 -> do x1 <- arbitrary
                           return (ParseError x1)
                   _ -> error "FATAL ERROR: Arbitrary instance, logic bug"


instance Arbitrary OperatorType where
        arbitrary
          = do x <- choose (0 :: Int, 3)
               case x of
                   0 -> return Prefix
                   1 -> return LeftAssoc
                   2 -> return RightAssoc
                   3 -> return VarAssoc
                   _ -> error "FATAL ERROR: Arbitrary instance, logic bug"


instance Arbitrary ImpId where
        arbitrary = return A


instance Arbitrary ImpRename where
        arbitrary = return B


instance Arbitrary SelectiveImport where
        arbitrary
          = do x <- choose (0 :: Int, 1)
               case x of
                   0 -> do x1 <- arbitrary
                           return (NoRename x1)
                   1 -> do x1 <- arbitrary
                           return (Rename x1)
                   _ -> error "FATAL ERROR: Arbitrary instance, logic bug"


instance Arbitrary Import where
        arbitrary
          = do x <- choose (0 :: Int, 3)
               case x of
                   0 -> do x1 <- arbitrary
                           return (Import x1)
                   1 -> do x1 <- arbitrary
                           return (ImportQualified x1)
                   2 -> do x1 <- arbitrary
                           x2 <- arbitrary
                           return (ImportQualifiedRename x1 x2)
                   3 -> do x1 <- arbitrary
                           x2 <- arbitrary
                           return (ImportFrom x1 x2)
                   _ -> error "FATAL ERROR: Arbitrary instance, logic bug"


instance Arbitrary ModuleAST where
        arbitrary
          = do x1 <- arbitrary
               x2 <- arbitrary
               x3 <- arbitrary
               x4 <- arbitrary
               x5 <- arbitrary
               x6 <- arbitrary
               x7 <- arbitrary
               return (ModuleAST x1 x2 x3 x4 x5 x6 x7)
