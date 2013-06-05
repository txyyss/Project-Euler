-- The radical of n, rad(n), is the product of distinct prime factors
-- of n. For example, 504 = 2³ × 3² × 7, so rad(504) = 2 × 3 × 7 = 42.

-- We shall define the triplet of positive integers (a, b, c) to be an
-- abc-hit if:

-- GCD(a, b) = GCD(a, c) = GCD(b, c) = 1
-- a < b
-- a + b = c
-- rad(abc) < c

-- For example, (5, 27, 32) is an abc-hit, because:

-- GCD(5, 27) = GCD(5, 32) = GCD(27, 32) = 1 5 < 27 5 + 27 = 32
-- rad(4320) = 30 < 32 It turns out that abc-hits are quite rare and
-- there are only thirty-one abc-hits for c < 1000, with ∑c = 12523.

-- Find ∑c for c < 120000.

module Euler127 where

import Data.Array
import Data.Ord
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

bnd = 120000

rads = listArray (1, bnd) $ map getFactors [1..bnd]
  where getFactors = helper primes
        removeFactor f n
          | n `mod` f == 0 = removeFactor f (n `div` f)
          | otherwise = n
        helper (y:ys) m
          | y * y > m = m
          | m `mod` y == 0 = y * (rads ! quotient)
          | otherwise = helper ys m
          where quotient = removeFactor y $ m `div` y

result127 = sum [c | (c, radC) <- sortedRads,
                 (a, radA) <- takeWhile (\(_,r) -> r * radC < (c `div` 2)) sortedRads,
                 let b = c - a, a < b, gcd a b == 1,
                 radA * rads ! b * radC < c]
  where sortedRads = sortBy (comparing snd) $ assocs rads
