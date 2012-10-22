-- The number 3797 has an interesting property. Being prime itself, it
-- is possible to continuously remove digits from left to right, and
-- remain prime at each stage: 3797, 797, 97, and 7. Similarly we can
-- work from right to left: 3797, 379, 37, and 3.

-- Find the sum of the only eleven primes that are both truncatable
-- from left to right and right to left.

-- NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.

module Euler037 where

import Data.List (union, inits, tails)
import Data.Char (digitToInt)

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

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = helper primes
  where helper (x:xs)
          | x * x > n = True
          | n `mod` x == 0 = False
          | otherwise = helper xs

isTruncatable :: Int -> Bool
isTruncatable n 
  | n < 10 = False
  | otherwise = and $ map (isPrime . (foldl1 (\x y -> x * 10 + y)) . map digitToInt) $ 
                union (init . tail . inits $ show n) (init . tail . tails $ show n)

findTruncatable :: Int -> [Int]
findTruncatable count = helper [] 0 primes
  where helper result n (x:xs)
          | n == count = result
          | isTruncatable x = helper (x:result) (n+1) xs
          | otherwise = helper result n xs

result037 = sum $ findTruncatable 11
