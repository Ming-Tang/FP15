{-# LANGUAGE Safe #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module FP15.Compiler.Syntax.Precedence (
  -- * Examples
  -- $examples

  -- * The Same Typeclass
  Same(key, same), Self(..),
  -- * Main Types
  Assoc(..), PrecNode(..), Tree(..), RightTree(..),
  PrecParseError(..),
  -- * Type Synonyms
  Prec, PrecRepr, Result,
  -- * Precdence Parsing
  insDefault,
  parsePrec,
  splitInfixNode,
  -- * Helpers
  showTree, showTree', showStringTree, getPrec
) where
import Control.Applicative
import Data.Maybe(mapMaybe)
import Data.List.Split(split, whenElt)
import Data.List
import FP15.Disp
import FP15.Types(Prec, Located, getLocated)
import qualified Data.Map as M

-- | The 'Same' typeclass is for determining the "identity" of an operator. This
-- typeclass is needed to deal with the fact that same operators with different
-- location info are considered the same.
--
-- All instances of the 'Same' typeclass must implement the 'key' function,
-- which is used to determine if two values are the 'same'.
class (Eq k, Ord k) => Same a k | a -> k where
  {-# MINIMAL key #-}
  key :: a -> k
  same :: a -> a -> Bool
  same a b = key a == key b

-- | The 'Self' type is an instance of 'Same' that has key of the value
-- itself.
newtype Self a = Self a
getSelf :: Self a -> a
getSelf (Self x) = x

instance (Eq a, Ord a) => Same (Self a) a where
  key = getSelf

instance (Eq a, Ord a) => Same (Located a) a where
  key = getLocated

instance Same Assoc Assoc where
  key = id

-- | Precedence are tie-broken by associativity.
type PrecRepr = (Prec, Maybe Assoc)

data Assoc = VarA | LeftA | RightA deriving (Eq, Ord, Show, Read)

data PrecNode o a = PreN Prec o | InfN Assoc Prec o | TermN a
                  deriving (Eq, Ord, Show, Read)

data Tree o a = Pre o (Tree o a)
              | Inf (Tree o a) o (Tree o a)
              | Var o [Tree o a]
              | Term (Maybe a)
              deriving (Eq, Ord, Show, Read)

data RightTree o a = Last [PrecNode o a]
                   | RBranch [PrecNode o a] (Assoc, o) (RightTree o a)
                   deriving (Eq, Ord, Show, Read)

data PrecParseError o a = ConsecutiveTerms ![PrecNode o a]
                        | MixingVarOp ![o] ![[PrecNode o a]]
                        deriving (Eq, Ord, Show, Read)

instance (Show o, Show a) => Disp (PrecParseError o a) where
  disp = show

type Result r o a = Either (PrecParseError o a) (r o a)

showTree :: (Show o, Show a) => Tree o a -> String
showTree = showTree' show show

showTree' :: (o -> String) -> (a -> String) -> Tree o a -> String
showTree' so sa = f where
  f (Pre o t) = "(" ++ so o ++ " " ++ f t ++ ")"
  f (Inf a o b) = "(" ++ f a ++ " " ++ so o ++ " " ++ f b ++ ")"
  f (Var o ts) = "(" ++ so o ++ " " ++ unwords (map f ts) ++ ")"
  f (Term Nothing) = "_"
  f (Term (Just x)) = sa x

showStringTree :: Tree String String -> String
showStringTree = showTree' id id

-- | The 'getPrec' function gets the precedence representation of a 'PrecNode'.
getPrec :: PrecNode o a -> Maybe PrecRepr
getPrec (PreN p _) = Just (p, Nothing)
getPrec (InfN a p _) = Just (p, Just a)
getPrec _ = Nothing

-- | The 'insDefault' function inserts a default infix operator between two
-- adjacent terms. Must be done because the precedence parsing functions cannot
-- handle two consecutive terms.
insDefault :: (Assoc, Prec, o) -> [PrecNode o a] -> [PrecNode o a]
insDefault (a, p, o) ns = ins (InfN a p o) ns []

ins :: PrecNode o a -> [PrecNode o a] -> [PrecNode o a] -> [PrecNode o a]
ins x [] !acc = acc
ins x [a] !acc = acc ++ [a]
ins x (a@(TermN _):b:cs) !acc | isTermOrPrefix b = ins x (b:cs) (acc ++ [a, x])
  where isTermOrPrefix (TermN _) = True
        isTermOrPrefix (PreN _ _) = True
        isTermOrPrefix _ = True
ins x (a:b:cs) !acc = ins x (b:cs) (acc ++ [a])

-- | The 'parsePrec' performs precedence parsing from a list of 'PrecNode's.
--
-- Precondition: @ns@ is a result of @insDefault@. In other words, @ns@ cannot
-- contain two consecutive 'TermN's.
parsePrec :: Same o k => [PrecNode o a] -> Result Tree o a
parsePrec ns = parsePrec' precs ns where
  precs = sort $ mapMaybe getPrec ns

parsePrec' :: Same o k => [PrecRepr] -> [PrecNode o a] -> Result Tree o a
parsePrec' (p2@(p, _):ps) (PreN p0 o : ns)
  | p0 < p = Pre o <$> parsePrec' (p2:ps) ns
parsePrec' (p:ps) ns = joinParts (parsePrec' ps) =<< splitInfixNode p ns

-- Base cases
parsePrec' _ [] = return $ Term Nothing
parsePrec' _ [TermN x] = return $ Term (Just x)
parsePrec' _ [InfN _ _ o] = return $ Inf (Term Nothing) o (Term Nothing)
parsePrec' _ [PreN _ o] = return $ Pre o (Term Nothing)
parsePrec' [] (PreN _ o : xs) = Pre o <$> parsePrec' [] xs
parsePrec' [] xs@(_:_) = Left $ ConsecutiveTerms xs

-- | Given a 'RightTree' of operators with same precedence, join them into a
-- 'Tree'.
--
-- Precondition: All nodes must have consistent associativity.
joinParts :: Same o k => ([PrecNode o a] -> Result Tree o a) -> RightTree o a -> Result Tree o a
joinParts f ps = let (xs, y) = toListR ps in
  case uniq $ map (\(_, a, _) -> a) xs of
    Left Nothing -> f y
    Left (Just VarA) ->
      -- TODO uniq doesn't work because all location info is unique
      case uniq $ map (\(_, _, o) -> o) xs of
        Left (Just o) -> do
          ys <- mapM (\(x, _, _) -> f x) xs
          z <- f y
          return $ Var o (ys ++ [z])
        Left Nothing ->
          error "FP15.Compiler.Precedence.joinParts: impossible"
        Right os ->
          Left $ MixingVarOp os $ map (\(a, _, _) -> a) xs ++ [y]
    Left (Just LeftA) -> do
      let (x0, xs') = toListL ps
      y0 <- f x0
      ys <- mapM (\(a0, o, b) -> f b >>= \b' -> return (a0, o, b')) xs'
      return $ foldl (\a (_, o, b') -> Inf a o b') y0 ys
    Left (Just RightA) -> do
      z0 <- f y
      ys <- mapM (\(a, a0, o) -> f a >>= \a' -> return (a', a0, o)) xs
      return $ foldr (\(a', _, o) b -> Inf a' o b) z0 ys
    Right _ ->
      error "FP15.Compiler.Precedence.joinParts: impossible: multiple assocs"

toListR :: RightTree o a -> ([([PrecNode o a], Assoc, o)], [PrecNode o a])
toListR (Last x) = ([], x)
toListR (RBranch x (a, o) t) = let (xs, y) = toListR t in ((x, a, o):xs, y)

toListL :: RightTree o a -> ([PrecNode o a], [(Assoc, o, [PrecNode o a])])
toListL (Last x) = (x, [])
toListL (RBranch x (a, o) t) = (x, ys) where
  ys = f (a, o) t
  f (a', o') (Last x') = [(a', o', x')]
  f (a', o') (RBranch x' (a'', o'') t') = (a', o', x'):f (a'', o'') t'


uniq :: Same a k => [a] -> Either (Maybe a) [a]
uniq l = case M.toList $ M.fromList $ map (\x -> (key x, x)) l of
            [] -> Left Nothing
            [(k0, x)] -> Left (Just x)
            ys -> Right $ map snd ys

-- | The 'splitInfixNode' split a list of @PrecNode@s by infix operators of the
-- given precedence.
--
-- >>> splitInfixNode (0, Just LeftA) [PreN 0 "+"]
-- Last [PreN 0 "+"]
-- >>> splitInfixNode (0, Just LeftA) [PreN 0 "+", TermN "x"]
-- Last [PreN 0 "+",TermN "x"]
-- >>> splitInfixNode (0, Just LeftA) [TermN "x", InfN LeftA 0 "+", TermN "y"]
-- RBranch [TermN "x"] (LeftA,"+") (Last [TermN "y"])
-- >>> splitInfixNode (0, Just LeftA) [TermN "x", InfN LeftA 0 "+", TermN "y", InfN LeftA 1 "*", TermN "z"]
-- RBranch [TermN "x"] (LeftA,"+") (Last [TermN "y",InfN LeftA 1 "*",TermN "z"])
splitInfixNode :: PrecRepr -> [PrecNode o a] -> Result RightTree o a
splitInfixNode p = parsePairs . split (whenElt $ isInfixNodeOf p) where
  parsePairs [x] = return $ Last x
  parsePairs (x:[InfN a' p' o]:xs)
    | p == (p', Just a') = RBranch x (a', o) <$> parsePairs xs
    | otherwise = error "FP15.Compiler.Precedence.splitInfixNode: impossible: wrong prec"
  parsePairs _ = error "FP15.Compiler.Precedence.splitInfixNode: impossible: empty list"

isInfixNodeOf :: PrecRepr -> PrecNode o a -> Bool
isInfixNodeOf p (InfN a p' _) = (p', Just a) == p
isInfixNodeOf _ _ = False

-- $examples
-- Here are some examples to demonstrate precedence parsing.
--
-- Some definitions to make the inputs more readable.
--
-- >>> let (t,l,r,v,p) = (TermN,InfN LeftA,InfN RightA,InfN VarA,PreN)
--
-- Left associative operator:
--
-- >>> showStringTree $ parsePrec [t"x", l 0 "+", t"y", l 0 "+", t"z", l 0 "-", t"w"]
-- "(((x + y) + z) - w)"
--
-- Right associative operator:
--
-- >>> showStringTree $ parsePrec [t"x", r 0 "+", t"y", r 0 "+", t"z", r 0 "-", t"w"]
-- "(x + (y + (z - w)))"
--
-- Variadic associative operator:
--
-- >>> showStringTree $ parsePrec [t"x", v 0 "><", t"y", v 0 "><", t"z", v 0 "><", t"w"]
-- "(>< x y z w)"
--
-- For same precedence level, right-associative has higher precedence and
-- variadic has lowest.
--
-- >>> showStringTree $ parsePrec [t"a", l 0 "->", t"b", l 0 "->", t"d", r 0 "<-", t"c", l 0 "->", t"e"]
-- "(((a -> b) -> (d <- c)) -> e)"

-- $doctests
-- >>> splitInfixNode (0, Just LeftA) [TermN "x", InfN LeftA 0 "+"]
-- RBranch [TermN "x"] (LeftA,"+") (Last [])
-- >>> splitInfixNode (0, Just LeftA) [InfN LeftA 0 "+"]
-- RBranch [] (LeftA,"+") (Last [])
-- >>> splitInfixNode (0, Just LeftA) [TermN "x", InfN LeftA 0 "+", TermN "y", InfN LeftA 0 "-", TermN "z"]
-- RBranch [TermN "x"] (LeftA,"+") (RBranch [TermN "y"] (LeftA,"-") (Last [TermN "z"]))
-- >>> splitInfixNode (0, Just LeftA) [TermN "x", InfN LeftA 0 "+", TermN "y", InfN LeftA 1 "*", TermN "z", InfN LeftA 0 "-"]
-- RBranch [TermN "x"] (LeftA,"+") (RBranch [TermN "y",InfN LeftA 1 "*",TermN "z"] (LeftA,"-") (Last []))
-- >>> splitInfixNode (0, Just LeftA) [TermN "x", InfN LeftA 0 "+", TermN "y", InfN LeftA 1 "*", InfN LeftA 0 "-"]
-- RBranch [TermN "x"] (LeftA,"+") (RBranch [TermN "y",InfN LeftA 1 "*"] (LeftA,"-") (Last []))
-- >>> splitInfixNode (0, Just LeftA) [TermN "x", InfN LeftA 0 "+", InfN LeftA 1 "*", TermN "z", InfN LeftA 0 "-"]
-- RBranch [TermN "x"] (LeftA,"+") (RBranch [InfN LeftA 1 "*",TermN "z"] (LeftA,"-") (Last []))
-- >>> splitInfixNode (1, Just LeftA) [PreN 1 "~", InfN LeftA 1 "*", TermN "z", InfN LeftA 1 "%"]
-- RBranch [PreN 1 "~"] (LeftA,"*") (RBranch [TermN "z"] (LeftA,"%") (Last []))
-- >>> splitInfixNode (1, Just LeftA) [TermN "x", InfN LeftA 0 "+", TermN "y"]
-- Last [TermN "x",InfN LeftA 0 "+",TermN "y"]
-- >>> splitInfixNode (0, Just LeftA) [InfN LeftA 0 "+", TermN "y"]
-- RBranch [] (LeftA,"+") (Last [TermN "y"])

