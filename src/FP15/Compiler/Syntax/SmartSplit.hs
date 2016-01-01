module FP15.Compiler.Syntax.SmartSplit (smartSplit) where
import Data.Maybe
import FP15.Types()

-- | The 'smartSplit' function splits a list so that each part satisfy the
-- predicate and the lengths of the left parts are greedily maximized. If there
-- is no solution, or the input list is empty, the result is 'Nothing'.
--
-- >>> smartSplit (const True) []
-- Nothing
-- >>> smartSplit (`elem` ["@","++"]) "@++"
-- Just ["@","++"]
-- >>> smartSplit (`elem` ["?","?<","?>",">","<",">=","<=","="]) "?<>="
-- Just ["?<",">="]
--
-- prop> xs /= [] ==> Nothing == smartSplit (const False) xs
-- prop> xs /= [] ==> Just [xs] == smartSplit (const True) xs
-- prop> xs /= [] ==> Just [xs] == smartSplit (== xs) xs
-- prop> concat ps /= [] ==> Just (concat ps) == fmap concat (smartSplit (const True) $ concat ps)
-- prop> xs /= [] ==> Just [xs] == smartSplit (`elem` map (`take` xs) [1..length xs]) xs
smartSplit :: ([a] -> Bool) -> [a] -> Maybe [[a]]
smartSplit _ [] = Nothing
smartSplit p xs
  | p xs = Just [xs]
  | otherwise = listToMaybe [ hd:st | (hd, tl) <- tail $ splits xs, p hd,
                                      st <- maybeToList $ smartSplit p tl ]

splits :: [a] -> [([a], [a])]
splits xs = map (`splitAt` xs) [length xs, length xs - 1..1]
