-- NOTE: This problem is a significantly more challenging version of
-- Problem 81.

-- In the 5 by 5 matrix below, the minimal path sum from the top left
-- to the bottom right, by moving left, right, up, and down, is
-- indicated in bold red and is equal to 2297.

-- 131	673	234	103	18
-- 201	96	342	965	150
-- 630	803	746	422	111
-- 537	699	497	121	956
-- 805	732	524	37	331

-- Find the minimal path sum, in matrix.txt (right click and 'Save
-- Link/Target As...'), a 31K text file containing a 80 by 80 matrix,
-- from the top left to the bottom right by moving left, right, up,
-- and down.

module Euler083 where

import Data.Array
import qualified Data.Set as Set
import Data.Ord
import Data.List

getMatrix :: String -> Array (Int,Int) Int
getMatrix content = listArray ((1,1),((length matrix), (length $ head matrix))) $ concat matrix
  where matrix = map (\x -> read $ "[" ++ x ++ "]") $ lines content

neighbourIndexes :: ((Int,Int),(Int,Int)) -> (Int,Int) -> [(Int,Int)]
neighbourIndexes ((rMin,cMin),(rMax,cMax)) (x,y) =
  filter (\(a,b)-> rMin <= a && a <= rMax && cMin <= b && b <= cMax) [(x,y+1),(x,y-1),(x+1,y),(x-1,y)]

-- findMinSum ::  (Int,Int) -> (Int,Int) -> Array (Int,Int) Int -> Int
findMinSum src tgt wMatrix = helper initMatrix initQ
  where bOfMa@((rMin,cMin),(rMax,cMax)) = bounds wMatrix
        initMatrix = (listArray bOfMa $ repeat (10^6)) // [(src, wMatrix ! src)]
        initQ = Set.fromList [(x,y) | x <-[rMin..rMax], y<-[cMin..cMax]]
        helper distanceMatrix qSet
          | Set.notMember tgt qSet = distanceMatrix ! tgt
          | otherwise = helper newDisMatrix newQSet
          where minU = fst . minimumBy (comparing snd) $ map (\x -> (x,distanceMatrix!x)) $ Set.toList qSet 
                newQSet = Set.delete minU qSet
                newDis :: [((Int,Int), Int)]
                newDis = map (\v -> (v, min (distanceMatrix ! v) (distanceMatrix ! minU + wMatrix ! v))) $
                         neighbourIndexes bOfMa minU
                newDisMatrix = distanceMatrix // newDis

result083 = fmap (findMinSum (1,1) (80,80) . getMatrix) $ readFile "data/matrix.txt"
