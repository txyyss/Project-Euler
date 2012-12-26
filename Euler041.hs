-- We shall say that an n-digit number is pandigital if it makes use
-- of all the digits 1 to n exactly once. For example, 2143 is a
-- 4-digit pandigital and is also prime.

-- What is the largest n-digit pandigital prime that exists?

module Euler041 where

import Data.List (sort)

minus :: Ord a => [a] -> [a] -> [a]
minus lx@(x:xs) ly@(y:ys) = case compare x y of 
  LT -> x : minus xs ly
  EQ ->     minus xs ys 
  GT ->     minus lx ys
minus xs _ = xs

primesTo :: Integral a => a -> [a]
primesTo m = 2 : sieve [3,5..m]  where
  sieve (p:xs) 
    | p*p > m   = p : xs
    | otherwise = p : sieve (xs `minus` [p*p, p*p+2*p..])

primes = primesTo 7654321

isPrime :: Int -> Bool
isPrime n = helper primes
  where helper [] = True
        helper (x:xs)
          | x * x > n = True
          | n `mod` x == 0 = False
          | otherwise = helper xs

isPandigital :: Int -> Bool
isPandigital n = sort showN == take len "123456789"
  where showN = show n
        len = length showN
        
result041 = head . filter isPrime $ filter isPandigital [7654321, 7654320..1]
