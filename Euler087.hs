-- The smallest number expressible as the sum of a prime square, prime
-- cube, and prime fourth power is 28. In fact, there are exactly four
-- numbers below fifty that can be expressed in such a way:

-- 28 = 2^2 + 2^3 + 2^4
-- 33 = 3^2 + 2^3 + 2^4
-- 49 = 5^2 + 2^3 + 2^4
-- 47 = 2^2 + 3^3 + 2^4

-- How many numbers below fifty million can be expressed as the sum of
-- a prime square, prime cube, and prime fourth power?

module Euler087 where

import Data.Array

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

numbersUnder :: Int -> [Int]
numbersUnder limit = [sq2 + sq3 + sq4 | sq4 <- l4, sq3 <- takeWhile (<limit - sq4) l3, sq2 <- takeWhile (<limit - sq4 -sq3) l2]
  where powerList n = takeWhile (<limit) $ map (^n) primes
        l2 = powerList 2
        l3 = powerList 3
        l4 = powerList 4

primeSumMap :: Array Int Bool
primeSumMap = accumArray (||) False (1,50000000) $ map (\x->(x, True)) $ numbersUnder 50000000

result087 = length $ filter (primeSumMap!) [1..50000000]
