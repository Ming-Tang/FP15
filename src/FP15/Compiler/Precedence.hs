{-# LANGUAGE BangPatterns #-}
module FP15.Compiler.Precedence (
  -- * Main Types
  Assoc(..), PrecNode(..), Tree(..), RightTree(..),
  -- * Type Synonyms
  Prec, PrecRepr,
  -- * Precdence Parsing
  insDefault,
  parsePrec,
  splitInfixNode,
  -- * Helpers
  showTree, showTree', showStringTree, getPrec
) where
import Data.Maybe(mapMaybe)
import Data.List.Split(split, whenElt)
import Data.List
import qualified Data.Set as S

-- | Operator precedence.
type Prec = Int

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
insDefault :: PrecNode o a -> [PrecNode o a] -> [PrecNode o a]
insDefault x ns = ins x ns []

ins :: PrecNode o a -> [PrecNode o a] -> [PrecNode o a] -> [PrecNode o a]
ins x [] !acc = acc
ins x [a] !acc = acc ++ [a]
ins x (a@(TermN _):b@(TermN _):cs) !acc = ins x (b:cs) (acc ++ [a, x])
ins x (a:b:cs) !acc = ins x (b:cs) (acc ++ [a])

-- | The 'parsePrec' performs precedence parsing from a list of 'PrecNode's.
--
-- Precondition: @ns@ is a result of @insDefault@. In other words, @ns@ cannot
-- contain two consecutive 'TermN's.
parsePrec :: Ord o => [PrecNode o a] -> Tree o a
parsePrec ns = parsePrec' precs ns where
  precs = sort $ mapMaybe getPrec ns

parsePrec' :: Ord o => [PrecRepr] -> [PrecNode o a] -> Tree o a
parsePrec' (p2@(p, _):ps) (PreN p0 o : ns)
  | p0 < p = Pre o $ parsePrec' (p2:ps) ns
parsePrec' (p:ps) ns = joinParts (parsePrec' ps) $ splitInfixNode p ns

-- Base cases
parsePrec' _ [] = Term Nothing
parsePrec' _ [TermN x] = Term (Just x)
parsePrec' _ [InfN _ _ o] = Inf (Term Nothing) o (Term Nothing)
parsePrec' _ [PreN _ o] = Pre o (Term Nothing)
parsePrec' [] (_:_) =
  error "FP15.Compiler.Precedence.parsePrec: empty prec with 2+ nodes"

-- | Given a 'RightTree' of operators with same precedence, join them into a
-- 'Tree'.
--
-- Precondition: All nodes must have consistent associativity.
joinParts :: Ord o => ([PrecNode o a] -> Tree o a) -> RightTree o a -> Tree o a
joinParts f ps = let (xs, y) = toListR ps in
  case uniq $ map (\(_, a, _) -> a) xs of
    Left Nothing -> f y
    Left (Just VarA) ->
      case uniq $ map (\(_, _, o) -> o) xs of
        Left (Just o) -> Var o (map (\(x, _, _) -> f x) xs ++ [f y])
        Left Nothing ->
          error "FP15.Compiler.Precedence.joinParts: impossible"
        Right _ ->
          error "FP15.Compiler.Precedence.joinParts: multiple variadic ops"
    Left (Just LeftA) ->
      let (x0, xs') = toListL ps in
      foldl (\a (_, o, b) -> Inf a o (f b)) (f x0) xs'
    Left (Just RightA) -> foldr (\(a, _, o) b -> Inf (f a) o b) (f y) xs
    Right _ -> error "FP15.Compiler.Precedence.joinParts: multiple assocs"

toListR :: RightTree o a -> ([([PrecNode o a], Assoc, o)], [PrecNode o a])
toListR (Last x) = ([], x)
toListR (RBranch x (a, o) t) = let (xs, y) = toListR t in ((x, a, o):xs, y)

toListL :: RightTree o a -> ([PrecNode o a], [(Assoc, o, [PrecNode o a])])
toListL (Last x) = (x, [])
toListL (RBranch x (a, o) t) = (x, ys) where
  ys = f (a, o) t
  f (a', o') (Last x') = [(a', o', x')]
  f (a', o') (RBranch x' (a'', o'') t') = (a', o', x'):f (a'', o'') t'


uniq :: Ord a => [a] -> Either (Maybe a) [a]
uniq l = case S.toList $ S.fromList l of
            [] -> Left Nothing
            [x] -> Left (Just x)
            ys -> Right ys

-- | The 'splitInfixNode' split a list of @PrecNode@s by infix operators of the
-- given precedence.
--
-- >>> splitInfixNode (0, Just LeftA) [PreN 0 "+"]
-- Last [PreN 0 "+"]
-- >>> splitInfixNode (0, Just LeftA) [PreN 0 "+", TermN "x"]
-- Last [PreN 0 "+",TermN "x"]
-- >>> splitInfixNode (0, Just LeftA) [TermN "x", InfN LeftA 0 "+", TermN "y"]
-- RBranch [TermN "x"] (LeftA,"+") (Last [TermN "y"])
-- >>> splitInfixNode (1, Just LeftA) [TermN "x", InfN LeftA 0 "+", TermN "y"]
-- Last [TermN "x",InfN LeftA 0 "+",TermN "y"]
-- >>> splitInfixNode (0, Just LeftA) [InfN LeftA 0 "+", TermN "y"]
-- RBranch [] (LeftA,"+") (Last [TermN "y"])
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
splitInfixNode :: PrecRepr -> [PrecNode o a] -> RightTree o a
splitInfixNode p = parsePairs . split (whenElt $ isInfixNodeOf p) where
  parsePairs [x] = Last x
  parsePairs (x:[InfN a' p' o]:xs)
    | p == (p', Just a') = RBranch x (a', o) (parsePairs xs)
    | otherwise = error "FP15.Compiler.Precedence.splitInfixNode: wrong prec"
  parsePairs _ = error "FP15.Compiler.Precedence.splitInfixNode: empty list"

isInfixNodeOf :: PrecRepr -> PrecNode o a -> Bool
isInfixNodeOf p (InfN a p' _) = (p', Just a) == p
isInfixNodeOf _ _ = False

