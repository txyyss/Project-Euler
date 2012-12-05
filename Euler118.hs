-- Using all of the digits 1 through 9 and concatenating them freely
-- to form decimal integers, different sets can be
-- formed. Interestingly with the set {2,5,47,89,631}, all of the
-- elements belonging to it are prime.

-- How many distinct sets containing each of the digits one through
-- nine exactly once contain only prime elements?

module Euler118 where

import Data.List
import qualified Data.Map as Map

minus :: Ord a => [a] -> [a] -> [a]
minus lx@(x:xs) ly@(y:ys) = case (compare x y) of 
  LT -> x : minus xs ly
  EQ ->     minus xs ys 
  GT ->     minus lx ys
minus xs _ = xs

primes :: [Integer]
primes = 2 : primes'
  where
    primes' = sieve [3,5..] 9 primes'
    sieve (x:xs) q ps@ ~(p:t)
      | x < q     = x : sieve xs q ps
      | otherwise =     sieve (xs `minus` [q, q+2*p..]) (head t^2) t

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime n = helper primes
  where helper (x:xs)
          | x * x > n = True
          | n `mod` x == 0 = False
          | otherwise = helper xs

primesCount :: Map.Map [Integer] Int
primesCount = Map.fromList [(x, length $ countPrime x) | x <- subsequences [1..9]]
  where countPrime x = [n | ls <- permutations x, let n = foldl1 (\a b -> a*10+b) ls, isPrime n]

partitionPrimesCount :: [Integer] -> Int
partitionPrimesCount = (Map.!) cache
  where cache = Map.fromList [(aSet, helper aSet) | aSet <- subsequences [1..9]]
        helper [] = 1
        helper (x:xs) = sum lst
          where lst = [primesCount Map.! (x:ss) * cache Map.! (xs \\ ss) | ss <- subsequences xs]

result118 = partitionPrimesCount [1..9]
