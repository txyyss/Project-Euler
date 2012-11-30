-- In the following equation x, y, and n are positive integers.

-- 1/x + 1/y=1/n

-- For n = 4 there are exactly three distinct solutions:

-- 1/5 + 1/20 = 1/4
-- 1/6 + 1/12 = 1/4
-- 1/8 + 1/8 = 1/4

-- What is the least value of n for which the number of distinct
-- solutions exceeds one-thousand?

-- NOTE: This problem is an easier version of problem 110; it is
-- strongly advised that you solve this one first.

module Euler108 where

import Data.List

-- see analysis in
-- http://www.mathblog.dk/project-euler-108-diophantine-equation/
-- find solution such that  (d(n^2) + 1) / 2 > 1000

minus :: Ord a => [a] -> [a] -> [a]
minus lx@(x:xs) ly@(y:ys) = case (compare x y) of 
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

factors :: Int -> [Int]
factors = helper [] primes
  where helper ls ps@(y:ys) m
          | y * y > m = m:ls
          | m `mod` y == 0 = helper (y:ls) ps (m `div` y)
          | otherwise = helper ls ys m

dSquare :: Int -> Int
dSquare = product . map ((\x -> 2 * x + 1) . length) . group . factors

result108 = fst . head . filter ((>1000) . (`div` 2) . (+1) . snd) . zip [4..] $ map dSquare [4..]
