module FP15.Types where
import FP15.Value(Value)

-- Name resolution types

data Functional
data Function

data Unresolved
data Resolved

class NameType a where
  nameType :: a

class Resolution a where
  resolution :: a

instance NameType Functional where
  nameType = undefined

instance NameType Function where
  nameType = undefined

instance Resolution Unresolved where
  resolution = undefined

instance Resolution Resolved where
  resolution = undefined

data Name t r = Unqualified String
              | Qualified [String] String
              deriving (Eq, Show, Read)

type UnresolvedFunction = Name Function Unresolved
type ResolvedFunction = Name Function Resolved
type UnresolvedFunctional = Name Functional Unresolved
type ResolvedFunctional = Name Functional Resolved

-- An FP15 expression
data Expr r = Const Value
            | App (Name Functional r) [Expr r]
            | Func (Name Function r)
            deriving (Eq, Show, Read)

