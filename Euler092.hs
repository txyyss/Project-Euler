-- A number chain is created by continuously adding the square of the
-- digits in a number to form a new number until it has been seen
-- before.

-- For example,

-- 44 -> 32 -> 13 -> 10 -> 1 -> 1
-- 85 -> 89 -> 145 -> 42 -> 20 -> 4 -> 16 -> 37 -> 58 -> 89

-- Therefore any chain that arrives at 1 or 89 will become stuck in an
-- endless loop. What is most amazing is that EVERY starting number
-- will eventually arrive at 1 or 89.

-- How many starting numbers below ten million will arrive at 89?

module Euler092 where

import Data.Char
import Data.Array

nextSquareSum :: Int -> Int
nextSquareSum = sum . map (^2) . map digitToInt . show

arrive89 :: Int -> Array Int Bool
arrive89 limit = result
  where result = listArray (1,limit) $ map reach89 [1..limit]
        reach89 n = helper n
          where helper 89 = True
                helper 1 = False
                helper x
                  | x < n = result ! x
                  | otherwise = helper (nextSquareSum x)

result092 = length $ filter id $ elems $ arrive89 10000000
