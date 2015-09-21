{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
module FP15.Name where
import GHC.Generics
import Control.DeepSeq
import FP15.Disp
import Data.List(intercalate)
import Text.PrettyPrint

-- * Names

-- | An identifier of any kind (function, functional, operator)
newtype Id a = Id String
             deriving (Eq, Ord, Show, Read, Generic)

-- | An FP15 module name.
newtype ModuleName = M [String] -- TODO maybe use NonEmpty?
                   deriving (Eq, Ord, Show, Read, Generic)

getId :: Id a -> String
getModuleName :: ModuleName -> [String]

getId (Id i) = i
getModuleName (M n) = n

-- | A fully-qualified name
data Name a = N ![String] !String
            deriving (Eq, Ord, Show, Read, Generic)

convName :: Name a -> Name b
convName (N a b) = N a b

instance NFData (Name a)

instance Disp (Id a) where
  disp = getId

instance Disp ModuleName where
  disp = intercalate "." . getModuleName

instance Disp (Name a) where
  disp (N [] x) = x
  disp (N ns x) = intercalate "." ns ++ "." ++ x

-- * Relative Names

-- | The 'RelName' type represents a name relative to an environment @e@.
data RelName f e
  -- | A name relative to an environment.
  = RN e (RName f)
  -- | A name that requies the syntax provider of the enviornment to resolve.
  | SN e String
  -- ^ An absolute name.
  | AN e (AName f)
  deriving (Eq, Ord, Show, Generic, Functor)

instance (NFData e, NFData f) => NFData (RelName e f) where

-- * Source Position

-- | The type of source positions, which locates the part of a source file by
-- position, line and column, with optional filename.
data SrcPos = SrcPos { position :: !Int
                     , line :: !Int
                     , column :: !Int
                     , file :: !(Maybe String) }
            deriving (Eq, Ord, Show, Read, Generic)

instance NFData SrcPos

instance Disp SrcPos where
  -- file:12:3
  disp (SrcPos p l c (Just f)) = f ++ ":" ++ show l ++ ":" ++ show c
  -- <unknown>:12:3
  disp (SrcPos p l c Nothing) = ":" ++ show l ++ ":" ++ show c

-- | A value with optional location information attached.
data Located a = Loc !(Maybe SrcPos) a
#ifdef CUSTOM_SHOW
               deriving (Eq, Ord, Read, Functor, Generic)

instance Show a => Show (Located a) where
  show (Loc _ a) = show a
#else
               deriving (Eq, Ord, Show, Read, Functor, Generic)
#endif
instance NFData a => NFData (Located a)

instance Disp a => Disp (Located a) where
  pretty (Loc _ a) = pretty a

getSrcPos :: Located a -> Maybe SrcPos
getLocated :: Located a -> a

getSrcPos (Loc spos _) = spos
getLocated (Loc _ x) = x

noLoc :: a -> Located a
noLoc = Loc Nothing

withSameLoc :: Located a -> b -> Located b
withSameLoc o x = fmap (const x) o

-- | Wrapper to tell 'Disp' to include location information.
newtype DispLoc a = DispLoc (Located a)
                  deriving (Eq, Ord, Show, Read, Functor, Generic)

instance Disp a => Disp (DispLoc a) where
  -- abc
  pretty (DispLoc (Loc Nothing a)) = pretty a
  -- abc@file:12:3
  pretty (DispLoc (Loc (Just l) a)) = pretty a <> text "@" <> pretty l

type LocId a = Located (Id a)
type LocName a r = Located (Name (a, r))
type ULocName = Located (Name Unknown)

type RLocName a = LocName a Rel
type ALocName a = LocName a Abs

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

-- | Name is relative.
data Rel
-- | Name is absolute.
data Abs

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

deriving instance Eq Rel
deriving instance Ord Rel
deriving instance Show Rel
deriving instance Read Rel

deriving instance Eq Abs
deriving instance Ord Abs
deriving instance Show Abs
deriving instance Read Abs

-- ** Type Aliases

type FId = Id F
type FlId = Id Fl
type FOpId = Id FOp
type FlOpId = Id FlOp

type RName a = Name (a, Rel)
type AName a = Name (a, Abs)

type FName r = Name (F, r)
type FlName r = Name (Fl, r)
type FOpName r = Name (FOp, r)
type FlOpName r = Name (FlOp, r)

type RFName = FName Rel
type RFlName = FlName Rel
type RFOpName = FOpName Rel
type RFlOpName = FlOpName Rel

type AFName = FName Abs
type AFlName = FlName Abs
type AFOpName = FOpName Abs
type AFlOpName = FlOpName Abs

type UName = Name Unknown
