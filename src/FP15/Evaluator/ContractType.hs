{-# LANGUAGE Safe, GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
-- |
-- The 'FP15.Evaluator.ContractType' module contains types and typeclasses
-- required for handling contracts. Contracts solves the problem of implementing
-- standard library functions in a dynamic language, where the input types must
-- be (1) validated and (2) converted to statically-typed values or (3) a helpful
-- error message is outputted, while (4) making the implementation readable.
--
-- The name "contract" is derived from Dr. Racket's contract system.
--
-- Using intermediate contract objects, coupled with typeclasses and GADTs, the
-- concerns of input validation and parsing are pushed behind the scenes.
-- However, FP15's approach is complex and is not appropriate when you are
-- writing youself a Scheme in 48 hours.
--
-- Interesting enough, in the Aeson library, @FromJSON@ and @ToJSON@ achieve the
-- same thing for JSON values, except in Aeson's design, there are no
-- intermediate contract objects. That is because in FP15, contract objects
-- is required to for detailed diagnostic messages.
module FP15.Evaluator.ContractType where
import Data.These(These(..))
import FP15.Value
import FP15.Evaluator.FPValue

-- * Helper Types
-- The helper types are building blocks for describing the input types for
-- FP15 standard library functions.

-- ** Number Tower

-- TODO refactor to use GADTs for these two types

-- | The 'NumTower' type represents a position on the numerical tower.
data NumTower = CharT | IntT | RealT deriving (Eq, Ord, Show, Read)
-- | A number tagged by its position on the numerical tower.
data Number = CharN Char | IntN Integer | RealN Double deriving (Eq, Show, Read)

instance FPValueConvertible Number where
  toFPValue (CharN i) = Char i
  toFPValue (IntN i) = Int i
  toFPValue (RealN r) = Real r

-- ** Wrappers
-- Wrappers solve the problems of different FP15 types corresponding to same
-- Haskell type, for example, symbols, strings and lists of characters
-- are all Haskell 'String's.

-- | Wrapper for the 'StringC' contract.
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

-- | The 'Contract' type represents a description of what an 'FPValue' can
-- be, and how it's converted to a Haskell type. A contract of type
-- @'Contract' t@ means an 'FPValue' that conforms the contract can be
-- decomposed into a Haskell value of t.
data Contract t where
  AnyC :: Contract FPValue -- ^ Accepts anything.
  RealWorldC :: Contract RealWorld -- ^ Accepts only the 'RealWorld'.
  ValueC :: Contract Value -- ^ Accepts any pure value ('Value'-compatible).

  BoolC :: Contract Bool

  CharC :: Contract Char
  IntC :: Contract Integer
  RealC :: Contract Double
  NumberC :: Contract Number -- ^ Accepts any numeric type.

  SymbolC :: Contract Sym
  StringC :: Contract Str

  ListC :: Contract a -> Contract [a] -- ^ Accepts any list.
  ConsC :: Contract a -> Contract [b] -> Contract (Cons a [b])
  EmptyC :: Contract ()

  Args2C :: Contract a -> Contract b -> Contract (a, b)
  Args3C :: Contract a -> Contract b -> Contract c -> Contract (a, b, c)
  Args4C :: Contract a -> Contract b -> Contract c -> Contract d
            -> Contract (a, b, c, d)
  Args5C :: Contract a -> Contract b -> Contract c -> Contract d -> Contract e
            -> Contract (a, b, c, d, e)

  AndC :: Contract a -> Contract b -> Contract (Both a b)
  OrC :: Contract a -> Contract b -> Contract (These a b)
  EitherC :: Contract a -> Contract b -> Contract (Either a b) -- TODO better name

deriving instance Eq (Contract t)
deriving instance Show (Contract t)
-- TODO Disp instance

-- ^ The 'ContractConvertible' typeclass indicates that a type @t@ has it
-- corresponding contract value of type @'Contract' t@.
--
-- See also: 'FPValueConvertible'.
class ContractConvertible t where
  asContract :: Contract t
default (FPValue)

instance ContractConvertible FPValue where
  asContract = AnyC

instance ContractConvertible RealWorld where
  asContract = RealWorldC

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

instance (ContractConvertible a, ContractConvertible b)
         => ContractConvertible (Either a b) where
  asContract = EitherC asContract asContract

instance (ContractConvertible a, ContractConvertible b)
         => ContractConvertible (These a b) where
  asContract = OrC asContract asContract

instance (ContractConvertible a, ContractConvertible b)
         => ContractConvertible (Both a b) where
  asContract = AndC asContract asContract

