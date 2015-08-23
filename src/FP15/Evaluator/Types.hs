{-# LANGUAGE Safe, GADTs, RankNTypes, ExistentialQuantification #-}
{-# LANGUAGE ImpredicativeTypes, StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances, DeriveGeneric #-}
module FP15.Evaluator.Types where
import GHC.Generics
import Control.DeepSeq
import Control.Monad.Error
import Data.These(These(..))
import Text.PrettyPrint
import FP15.Disp
import FP15.Name
import FP15.Value

-- * Expression

type Ident = String
data BaseExpr = Const Value
              | Func Ident
              | Compose [BaseExpr]
              | If BaseExpr BaseExpr BaseExpr
              | Fork [BaseExpr]
              | Hook [BaseExpr]
              | Map BaseExpr
              | Filter BaseExpr
              | While BaseExpr BaseExpr
              | Mark Ident BaseExpr
              deriving (Eq, Show, Read, Generic)

instance NFData BaseExpr

-- * Error and Diagnostics

data StackFrame = StackFrame (Maybe (Located String))
                deriving (Eq, Ord, Show, Read)
newtype StackTrace = StackTrace [StackFrame]
                   deriving (Eq, Ord, Show, Read)

emptyStackTrace :: StackTrace
emptyStackTrace = StackTrace []

-- | An FP15 runtime error.
data RuntimeError = forall a. ContractViolation { contractViolated :: Contract a
                                                , offendingValue :: Value
                                                , stackTrace :: StackTrace }
                  | PassMismatchError { expectedLength :: Int
                                      , actualLength :: Int
                                      , stackTrace :: StackTrace }
                  | ErrorMessage { messageText :: String
                                 , stackTrace :: StackTrace }

deriving instance Show RuntimeError

instance Error RuntimeError where
  strMsg s = ErrorMessage s emptyStackTrace
  noMsg = ErrorMessage "Runtime error." emptyStackTrace

instance Disp StackTrace where
  pretty (StackTrace st) =
    joinLines $ text "Stack Trace:" : map pretty (reverse st)

instance Disp StackFrame where
  pretty (StackFrame func) =
    nest 2 $ maybe (text "<func unknown>") pretty func

instance Disp RuntimeError where
  pretty (ContractViolation c v st) =
    joinLines [text "Contract Violation:",
               text "Contract: " <> text (show c),
               text "Value: " <> text (disp v),
               pretty st]

  pretty (PassMismatchError m n st) =
    joinLines [text ("Pass: Arity mismatch: Expecting " ++ show m ++ " args"),
               text ("but got " ++ show n ++ "."),
               text (show st)]

  pretty (ErrorMessage s st) =
    joinLines [text "Error:" <+> text s, text (show st)]

joinLines :: [Doc] -> Doc
joinLines = vcat

-- TODO use continuation-based monad
type ResultOf = Either RuntimeError
type Result = Either RuntimeError Value

-- | An FP15 function, which takes a 'Value' and returns a 'Value' or a
-- 'RuntimeError'.
type Func = Value -> Result

-- | The 'NumTower' type represents a position on the numerical tower.
data NumTower = CharT | IntT | RealT deriving (Eq, Ord, Show, Read)
-- | A number tagged by its position on the numerical tower.
data Number = CharN Char | IntN Integer | RealN Double deriving (Eq, Show, Read)

instance ValueConvertible Number where
  toValue (CharN i) = Char i
  toValue (IntN i) = Int i
  toValue (RealN r) = Real r

-- * Wrapper Types

-- | Wrapper for the 'NotC' contract.
newtype Never a = Never { getNever :: Value } deriving (Eq, Show, Read)
-- | Wrapper for the 'StringC' contract
newtype Str = Str { getStr :: String } deriving (Eq, Show, Read)
-- | Wrapper for the 'SymbolC' contract.
newtype Sym = Sym { getSym :: String } deriving (Eq, Show, Read)
-- | Wrapper for the 'ConsC' contract.
newtype Cons a b = Cons { getCons :: (a, b) } deriving (Eq, Show, Read)
-- | Wrapper for the 'AndC' contract.
newtype Both a b = Both { getBoth :: (a, b) } deriving (Eq, Show, Read)

instance ValueConvertible Str where
  toValue = String . getStr

instance ValueConvertible Sym where
  toValue = Symbol . getSym

-- * Contract Type

-- | The 'Contract' type represents a description of what an FP15 'Value' can
-- be, and the decomposition of it to Haskell type.  A contract of type
-- @'Contract' t@ means all FP15 'Value' that conform to the contract can be
-- decomposed into a Haskell value of t.
data Contract t where
  AnyC :: Contract Value

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
default (Value)

instance ContractConvertible Value where
  asContract = AnyC

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
