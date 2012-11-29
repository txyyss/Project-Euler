-- Using network.txt (right click and 'Save Link/Target As...'), a 6K
-- text file containing a network with forty vertices, and given in
-- matrix form, find the maximum saving which can be achieved by
-- removing redundant edges whilst ensuring that the network remains
-- connected.

module Euler107 where

-- use Kruskal's algorithm

import Data.List
import Data.Ord
import Data.Set (Set)
import qualified Data.Set as Set

wordsBy :: Char -> String -> [String]
wordsBy c = helper [] ""
  where helper already current [] = reverse (reverse current : already)
        helper already current (x:xs)
          | x == c = helper (reverse current:already) "" xs
          | otherwise = helper already (x:current) xs

genEdges :: Int -> [String] -> [((Int,Int),Int)]
genEdges i = concatMap helper . zip [i..] . drop (i-1)
  where helper (_,"-") = [] 
        helper (x,s) = [((i,x),read s)]

kruskal :: [((Int,Int),Int)] -> Int
kruskal ls = helper 0 ls treeSet
  where edgePair = map fst ls
        totalSum = sum $ map snd ls
        treeSet = Set.fromList (map (Set.singleton . fst) edgePair ++ map (Set.singleton . snd) edgePair)
        helper spanSum lst aSet
          | lst == [] || Set.size aSet == 1 = totalSum - spanSum
          | otherwise = helper (spanSum + possibleWeight) (tail lst) newSet
          where ((v1, v2), weight) = head lst
                v1Set = Set.filter (Set.member v1) aSet
                v2Set = Set.filter (Set.member v2) aSet
                v1v2Same = v1Set == v2Set
                possibleWeight = if v1v2Same then 0 else weight
                v1v2Union = Set.union v1Set v2Set
                v1v2Merge = Set.unions $ Set.toList v1v2Union
                newSet = if v1v2Same
                         then aSet
                         else Set.insert v1v2Merge (Set.difference aSet v1v2Union)

result107 = fmap (kruskal . sortBy (comparing snd). concatMap (uncurry genEdges) . zip [1..] . map (wordsBy ',') . lines) $
            readFile "data/network.txt"
