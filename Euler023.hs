-- A perfect number is a number for which the sum of its proper
-- divisors is exactly equal to the number. For example, the sum of
-- the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which
-- means that 28 is a perfect number.

-- A number n is called deficient if the sum of its proper divisors is
-- less than n and it is called abundant if this sum exceeds n.

-- As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the
-- smallest number that can be written as the sum of two abundant
-- numbers is 24. By mathematical analysis, it can be shown that all
-- integers greater than 28123 can be written as the sum of two
-- abundant numbers. However, this upper limit cannot be reduced any
-- further by analysis even though it is known that the greatest
-- number that cannot be expressed as the sum of two abundant numbers
-- is less than this limit.

-- Find the sum of all the positive integers which cannot be written
-- as the sum of two abundant numbers.

module Euler where

import Data.Array

properDivisors :: Int -> [Int]
properDivisors n = tryDivide [1] [2..(floor $ sqrt $ fromIntegral n)]
  where tryDivide currRslt [] = currRslt
        tryDivide currRslt (x:xs)
          | n `mod` x == 0 = if (x == remain && x * remain == n) 
                             then tryDivide (x:currRslt) xs 
                             else tryDivide (x:remain:currRslt) xs
          | otherwise = tryDivide currRslt xs
          where remain = n `div` x

abundantNum :: Int -> Bool
abundantNum n = n < (sum $ properDivisors n)

abundants = listArray (1, 28123) $ map abundantNum [1..28123]

notSumOfAbundants :: Int -> Bool
notSumOfAbundants m = not $ or $ map (\(a, b) -> abundants ! a && abundants ! b) [(x, m-x) | x <- [1.. (m `div` 2)]]

result023 = sum $ filter notSumOfAbundants [1..28123]
