-- The radical of n, rad(n), is the product of distinct prime factors
-- of n. For example, 504 = 2³ × 3² × 7, so rad(504) = 2 × 3 × 7 = 42.

-- If we calculate rad(n) for 1 ≤ n ≤ 10, then sort them on rad(n),
-- and sorting on n if the radical values are equal, we get a list: 1
-- 2 4 8 3 9 5 6 7 10

-- Let E(k) be the kth element in the sorted n column; for example,
-- E(4) = 8 and E(6) = 9.

-- If rad(n) is sorted for 1 ≤ n ≤ 100000, find E(10000).

module Euler124 where

import Data.List

minus :: Ord a => [a] -> [a] -> [a]
minus lx@(x:xs) ly@(y:ys) = case compare x y of 
  LT -> x : minus xs ly
  EQ ->     minus xs ys 
  GT ->     minus lx ys
minus xs _ = xs

primes = 2 : primes'
  where
    primes' = sieve [3,5..] 9 primes'
    sieve (x:xs) q ps@ ~(p:t)
      | x < q     = x : sieve xs q ps
      | otherwise =     sieve (xs `minus` [q, q+2*p..]) (head t^2) t

distinctFactors :: Int -> [Int]
distinctFactors = helper [] primes
  where helper ls ps@(y:ys) m
          | y * y > m = m:ls
          | m `mod` y == 0 = helper (y:ls) ps (removeFactor y $ m `div` y)
          | otherwise = helper ls ys m
        removeFactor f n
          | n `mod` f == 0 = removeFactor f (n `div` f)
          | otherwise = n

rad = product . distinctFactors

result124 = e !! 9999
  where total = 100000
        totalR = [1..total]
        e = sort $ zip (map rad totalR) totalR
