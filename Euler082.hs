-- NOTE: This problem is a more challenging version of Problem 81.

-- The minimal path sum in the 5 by 5 matrix below, by starting in any
-- cell in the left column and finishing in any cell in the right
-- column, and only moving up, down, and right, is indicated in red
-- and bold; the sum is equal to 994.

-- 131	673	234	103	18
-- 201	96	342	965	150
-- 630	803	746	422	111
-- 537	699	497	121	956
-- 805	732	524	37	331

-- Find the minimal path sum, in matrix.txt (right click and 'Save
-- Link/Target As...'), a 31K text file containing a 80 by 80 matrix,
-- from the left column to the right column.

module Euler082 where

import Data.List

getMatrix :: String -> [[Int]]
getMatrix = transpose . map (\x -> read $ "[" ++ x ++ "]") . lines

minPathSum :: [[Int]] -> Int
minPathSum ls@(m:_) = helper (replicate (length m) 0) $ ls
  where helper row [] = minimum row
        helper row (x:xs) = helper result xs
          where lsPlus = zipWith (+) row x
                iterMin a (b,c) = min (a+b) c
                midResult = tail . scanl iterMin (head row) $ zip x lsPlus
                result = init . scanr (flip iterMin) (last row) $ zip x midResult

result082 = fmap (minPathSum . getMatrix) $ readFile "data/matrix.txt"
