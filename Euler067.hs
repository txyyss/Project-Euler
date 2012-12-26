-- By starting at the top of the triangle below and moving to adjacent
-- numbers on the row below, the maximum total from top to bottom is
-- 23.

-- 3
-- 7 4
-- 2 4 6
-- 8 5 9 3

-- That is, 3 + 7 + 4 + 9 = 23.

-- Find the maximum total from top to bottom in triangle.txt (right
-- click and 'Save Link/Target As...'), a 15K text file containing a
-- triangle with one-hundred rows.

-- NOTE: This is a much more difficult version of Problem 18. It is
-- not possible to try every route to solve this problem, as there are
-- 299 altogether! If you could check one trillion (10^12) routes
-- every second it would take over twenty billion years to check them
-- all. There is an efficient algorithm to solve it. ;o)

module Euler067 where

import Data.Char

parseTriangle :: String -> [[Int]]
parseTriangle str = map helper $ lines str
  where helper aLine = map toInt $ words aLine
        toInt = foldl1 (\x y -> x * 10 + y) . map digitToInt

findMaximumSum :: [[Int]] -> Int
findMaximumSum input = maximum $ getMax 0 (length input - 1) (head input)
  where getMax from to currentMax
          | from == to = currentMax
          | otherwise = getMax (from + 1) to $ map choose [0..(from + 1)]
          where wait = input !! (from + 1)
                choose i
                  | i == 0 = head currentMax + head wait
                  | i == (from + 1) = last currentMax + last wait
                  | otherwise = max (currentMax !! (i-1)) (currentMax !! i) + (wait !! i)
    
result067 = do
  content <- readFile "data/triangle.txt"
  return . findMaximumSum $ parseTriangle content
