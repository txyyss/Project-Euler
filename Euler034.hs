-- 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

-- Find the sum of all numbers which are equal to the sum of the
-- factorial of their digits.

-- Note: as 1! = 1 and 2! = 2 are not sums they are not included.

module Euler034 where

import Data.Array
import Data.Char (digitToInt)

count = listArray (0,9) $ scanl (*) 1 [1..9]

facSum :: Int -> Int
facSum = sum . map (\x-> count ! (digitToInt x)) . show

result034 = sum $ filter (\x -> x == facSum x) [10..10^6]
