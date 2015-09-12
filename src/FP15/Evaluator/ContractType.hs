{-# LANGUAGE Safe, GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module FP15.Evaluator.ContractType  where
import Data.These(These(..))
import FP15.Value
import FP15.Evaluator.FPValue

-- | The 'NumTower' type represents a position on the numerical tower.
data NumTower = CharT | IntT | RealT deriving (Eq, Ord, Show, Read)
-- | A number tagged by its position on the numerical tower.
data Number = CharN Char | IntN Integer | RealN Double deriving (Eq, Show, Read)

instance FPValueConvertible Number where
  toFPValue (CharN i) = Char i
  toFPValue (IntN i) = Int i
  toFPValue (RealN r) = Real r

-- * Wrapper Types

-- | Wrapper for the 'NotC' contract.
newtype Never a = Never { getNever :: FPValue }
-- | Wrapper for the 'StringC' contract
newtype Str = Str { getStr :: String } deriving (Eq, Show, Read)
-- | Wrapper for the 'SymbolC' contract.
newtype Sym = Sym { getSym :: String } deriving (Eq, Show, Read)
-- | Wrapper for the 'ConsC' contract.
newtype Cons a b = Cons { getCons :: (a, b) } deriving (Eq, Show, Read)
-- | Wrapper for the 'AndC' contract.
newtype Both a b = Both { getBoth :: (a, b) } deriving (Eq, Show, Read)

instance FPValueConvertible Str where
  toFPValue = String . getStr

instance FPValueConvertible Sym where
  toFPValue = Symbol . getSym

-- * Contract Type

-- | The 'Contract' type represents a description of what an FP15 'Value' can
-- be, and the decomposition of it to Haskell type.  A contract of type
-- @'Contract' t@ means all FP15 'Value' that conform to the contract can be
-- decomposed into a Haskell value of t.
data Contract t where
  AnyC :: Contract FPValue
  ValueC :: Contract Value

  BoolC :: Contract Bool

  CharC :: Contract Char
  IntC :: Contract Integer
  RealC :: Contract Double
  NumberC :: Contract Number

  SymbolC :: Contract Sym
  StringC :: Contract Str

  ListC :: Contract a -> Contract [a]
  NonEmptyListC :: Contract a -> Contract [a]

  EmptyC :: Contract ()
  ConsC :: Contract a -> Contract [b] -> Contract (Cons a [b])

  Args2C :: Contract a -> Contract b -> Contract (a, b)
  Args3C :: Contract a -> Contract b -> Contract c -> Contract (a, b, c)
  Args4C :: Contract a -> Contract b -> Contract c -> Contract d
            -> Contract (a, b, c, d)
  Args5C :: Contract a -> Contract b -> Contract c -> Contract d -> Contract e
            -> Contract (a, b, c, d, e)

  NotC :: Contract a -> Contract (Never a)
  AndC :: Contract a -> Contract b -> Contract (Both a b)
  OrC :: Contract a -> Contract b -> Contract (These a b)
  XorC :: Contract a -> Contract b -> Contract (Either a b)

deriving instance Eq (Contract t)
deriving instance Show (Contract t)

class ContractConvertible t where
  asContract :: Contract t
default (FPValue)

instance ContractConvertible FPValue where
  asContract = AnyC

instance ContractConvertible Value where
  asContract = ValueC

instance ContractConvertible Bool where
  asContract = BoolC

instance ContractConvertible Char where
  asContract = CharC

instance ContractConvertible Integer where
  asContract = IntC

instance ContractConvertible Double where
  asContract = RealC

instance ContractConvertible Number where
  asContract = NumberC

instance ContractConvertible Sym where
  asContract = SymbolC

instance (ContractConvertible a, ContractConvertible b)
         => ContractConvertible (Cons a [b]) where
  asContract = ConsC asContract asContract

instance ContractConvertible Str where
  asContract = StringC

instance ContractConvertible a => ContractConvertible [a] where
  asContract = ListC asContract

instance (ContractConvertible a, ContractConvertible b)
         => ContractConvertible (a, b) where
  asContract = Args2C asContract asContract

instance (ContractConvertible a, ContractConvertible b, ContractConvertible c)
         => ContractConvertible (a, b, c) where
  asContract = Args3C asContract asContract asContract

instance (ContractConvertible a, ContractConvertible b, ContractConvertible c,
          ContractConvertible d)
         => ContractConvertible (a, b, c, d) where
  asContract = Args4C asContract asContract asContract asContract

instance (ContractConvertible a, ContractConvertible b, ContractConvertible c,
          ContractConvertible d, ContractConvertible e)
         => ContractConvertible (a, b, c, d, e) where
  asContract = Args5C asContract asContract asContract asContract asContract

instance (ContractConvertible a) => ContractConvertible (Never a) where
  asContract = NotC asContract

instance (ContractConvertible a, ContractConvertible b)
         => ContractConvertible (Either a b) where
  asContract = XorC asContract asContract

instance (ContractConvertible a, ContractConvertible b)
         => ContractConvertible (These a b) where
  asContract = OrC asContract asContract

instance (ContractConvertible a, ContractConvertible b)
         => ContractConvertible (Both a b) where
  asContract = AndC asContract asContract

