-- A unit fraction contains 1 in the numerator. The decimal
-- representation of the unit fractions with denominators 2 to 10 are
-- given:

-- 1/2	= 	0.5
-- 1/3	= 	0.(3)
-- 1/4	= 	0.25
-- 1/5	= 	0.2
-- 1/6	= 	0.1(6)
-- 1/7	= 	0.(142857)
-- 1/8	= 	0.125
-- 1/9	= 	0.(1)
-- 1/10	= 	0.1

-- Where 0.1(6) means 0.166666..., and has a 1-digit recurring
-- cycle. It can be seen that 1/7 has a 6-digit recurring cycle.

-- Find the value of d 1000 for which 1/d contains the longest
-- recurring cycle in its decimal fraction part.

module Euler where

import Data.List (maximumBy)
import Data.Ord (comparing)

indexOf :: Eq a => a -> [a] -> Int
indexOf x xs = indexHelper 0 xs
  where indexHelper ind [] = ind
        indexHelper ind (y:ys)
          | y == x = ind
          | otherwise = indexHelper (ind + 1) ys

repeatingDecimalCount :: Int -> Int
repeatingDecimalCount n = countHelper [] 1
  where countHelper _ 0 = 0
        countHelper existed remain 
          | remainIdx < len = remainIdx + 1
          | otherwise = countHelper (remain:existed) ((remain * 10) `mod` n)
          where remainIdx = indexOf remain existed
                len = length existed

result026 = maximumBy (comparing snd) $ zip [1..] $ map repeatingDecimalCount [1..999]
