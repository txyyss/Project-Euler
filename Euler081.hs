-- In the 5 by 5 matrix below, the minimal path sum from the top left
-- to the bottom right, by only moving to the right and down, is
-- indicated in bold red and is equal to 2427.

-- 131	673	234	103	18
-- 201	96	342	965	150
-- 630	803	746	422	111
-- 537	699	497	121	956
-- 805	732	524	37	331

-- Find the minimal path sum, in matrix.txt (right click and 'Save
-- Link/Target As...'), a 31K text file containing a 80 by 80 matrix,
-- from the top left to the bottom right by only moving right and
-- down.

module Euler081 where

minPathSum :: [[Int]] -> Int
minPathSum (m:ms) = helper (scanl1 (+) m) ms
  where helper row [] = last row
        helper row (x:xs) = helper result xs
          where result = tail . scanl iterMin (head row) $ zip x lsPlus
                lsPlus = zipWith (+) row x
                iterMin a (b,c) = min (a+b) c

getMatrix :: String -> [[Int]]
getMatrix = map (\x -> read $ "[" ++ x ++ "]") . lines

result081 = fmap (minPathSum . getMatrix) $ readFile "data/matrix.txt"
