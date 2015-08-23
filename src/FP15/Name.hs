{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
module FP15.Name where
import FP15.Disp
import Data.List(intercalate)
import Text.PrettyPrint

-- * Names

-- | An identifier of any kind (function, functional, operator)
newtype Id a = Id String
             deriving (Eq, Ord, Show, Read)

-- | An FP15 module name.
newtype ModuleName = M [String]
                   deriving (Eq, Ord, Show, Read)

getId :: Id a -> String
getModuleName :: ModuleName -> [String]

getId (Id i) = i
getModuleName (M n) = n

-- | A fully-qualified name
data Name a = N ![String] !String
            deriving (Eq, Ord, Show, Read)

convName :: Name a -> Name b
convName (N a b) = N a b

instance Disp (Id a) where
  disp = getId

instance Disp ModuleName where
  disp = intercalate "." . getModuleName

instance Disp (Name a) where
  disp (N [] x) = x
  disp (N ns x) = intercalate "." ns ++ "." ++ x

-- * Source Position

-- | The type of source positions, which locates the part of a source file by
-- position, line and column, with optional filename.
data SrcPos = SrcPos { position :: !Int
                     , line :: !Int
                     , column :: !Int
                     , file :: !(Maybe String) }
            deriving (Eq, Ord, Show, Read)

instance Disp SrcPos where
  -- file:12:3
  disp (SrcPos p l c (Just f)) = f ++ ":" ++ show l ++ ":" ++ show c
  -- <unknown>:12:3
  disp (SrcPos p l c Nothing) = "<unknown>:" ++ show l ++ ":" ++ show c

-- | A value with optional location information attached.
data Located a = Loc !(Maybe SrcPos) a
               deriving (Eq, Ord, Show, Read, Functor)

instance Disp a => Disp (Located a) where
  -- abc
  pretty (Loc Nothing a) = pretty a <> text "@-"
  -- abc@file:12:3
  pretty (Loc (Just l) a) = pretty a <> text "@" <> pretty l

getSrcPos :: Located a -> Maybe SrcPos
getLocated :: Located a -> a

getSrcPos (Loc spos _) = spos
getLocated (Loc _ x) = x

noLoc :: a -> Located a
noLoc = Loc Nothing

withSameLoc :: Located a -> b -> Located b
withSameLoc o x = fmap (const x) o

type LocId a = Located (Id a)
type LocName a = Located (Name a)

-- * Type Tags
-- These empty datatypes are for tagging name types, to prevent programming
-- mistakes of passing a function name into a place where a functional name is
-- required.

-- | Function.
data F
-- | Functional.
data Fl
-- | Function operator.
data FOp
-- | Functional operator.
data FlOp
-- | Either function or functional operator.
data Unknown

deriving instance Eq F
deriving instance Ord F
deriving instance Show F
deriving instance Read F

deriving instance Eq Fl
deriving instance Ord Fl
deriving instance Show Fl
deriving instance Read Fl

deriving instance Eq FOp
deriving instance Ord FOp
deriving instance Show FOp
deriving instance Read FOp

deriving instance Eq FlOp
deriving instance Ord FlOp
deriving instance Show FlOp
deriving instance Read FlOp

deriving instance Eq Unknown
deriving instance Ord Unknown
deriving instance Show Unknown
deriving instance Read Unknown

-- ** Type Aliases

type FId = Id F
type FlId = Id Fl
type FOpId = Id FOp
type FlOpId = Id FlOp

type FName = Name F
type FlName = Name Fl
type FOpName = Name FOp
type FlOpName = Name FlOp

type UnknownName = Name Unknown
