-- Let p(n) represent the number of different ways in which n coins
-- can be separated into piles. For example, five coins can separated
-- into piles in exactly seven different ways, so p(5)=7.

-- OOOOO
-- OOOO   O
-- OOO   OO
-- OOO   O   O
-- OO   OO   O
-- OO   O   O   O
-- O   O   O   O   O

-- Find the least value of n for which p(n) is divisible by one
-- million.

module Euler078 where

import Data.Array

-- see http://en.wikipedia.org/wiki/Partition_(number_theory)#Exact_formula

pentagonals = [k * (3 * k - 1) `div` 2 | k <- concatMap (\x -> [x,-x]) [1..]]

partitions :: Int -> Array Int Integer
partitions limit = result
  where result = listArray (0,limit) $ map helper [0..limit]
        helper 0 = 1
        helper n = sum [sgn * result ! (n-gk) | (sgn, gk) <- zip (cycle [1,1,-1,-1]) $ takeWhile (<=n) pentagonals]

findMod :: Integer -> Int
findMod m = helper 1
  where helper n
          | lst == [] = helper (2 * n)
          | otherwise = head lst
          where lst = filter (\x -> (partsCount ! x) `mod` m == 0) [1..n] 
                partsCount = partitions n

result078 = findMod 1000000
