-- The number, 197, is called a circular prime because all rotations
-- of the digits: 197, 971, and 719, are themselves prime.

-- There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17,
-- 31, 37, 71, 73, 79, and 97.

-- How many circular primes are there below one million?

module Euler where

import Data.Char (digitToInt)

minus :: Ord a => [a] -> [a] -> [a]
minus lx@(x:xs) ly@(y:ys) = case (compare x y) of 
  LT -> x : minus xs ly
  EQ ->     minus xs ys 
  GT ->     minus lx ys
minus xs _ = xs

primesTo :: Integral a => a -> [a]
primesTo m = 2 : sieve [3,5..m]  where
  sieve (p:xs) 
    | p*p > m   = p : xs
    | otherwise = p : sieve (xs `minus` [p*p, p*p+2*p..])

primes = primesTo 1000000

isPrime :: Int -> Bool
isPrime n = helper primes
  where helper [] = True
        helper (x:xs)
          | x * x > n = True
          | n `mod` x == 0 = False
          | otherwise = helper xs
                        
rotations :: [a] -> [[a]]
rotations l = helper [] len (cycle l)
  where len = length l
        helper result 0 _ = result
        helper result n cs = helper ((take len cs):result) (n - 1) (tail cs)

isCircularPrime :: Int -> Bool
isCircularPrime n = and . map (isPrime . (foldl1 (\x y -> 10 * x + y)) . map digitToInt) . rotations $ show n

result035 = length $ filter isCircularPrime primes
