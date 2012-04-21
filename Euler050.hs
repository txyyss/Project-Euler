-- The prime 41, can be written as the sum of six consecutive primes:

-- 41 = 2 + 3 + 5 + 7 + 11 + 13

-- This is the longest sum of consecutive primes that adds to a prime
-- below one-hundred.

-- The longest sum of consecutive primes below one-thousand that adds
-- to a prime, contains 21 terms, and is equal to 953.

-- Which prime, below one-million, can be written as the sum of the
-- most consecutive primes?

module Euler where

import Data.List (maximumBy)
import Data.Ord (comparing)

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
                        
maxPairFor :: [Int] -> (Int, Int)
maxPairFor = last . filter (isPrime . snd) . zip [1..] . takeWhile (<1000000) . scanl1 (+)

minLen = fst $ maxPairFor primes

dropLimit = fst . last . zip [0..] . takeWhile (<1000000) $ map (\n -> sum . take minLen $ drop n primes) [0..]

result050 = snd . maximumBy (comparing fst) $ map (\n -> maxPairFor $ drop n primes) [0..dropLimit]
