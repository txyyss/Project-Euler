-- The proper divisors of a number are all the divisors excluding the
-- number itself. For example, the proper divisors of 28 are 1, 2, 4,
-- 7, and 14. As the sum of these divisors is equal to 28, we call it
-- a perfect number.

-- Interestingly the sum of the proper divisors of 220 is 284 and the
-- sum of the proper divisors of 284 is 220, forming a chain of two
-- numbers. For this reason, 220 and 284 are called an amicable pair.

-- Perhaps less well known are longer chains. For example, starting
-- with 12496, we form a chain of five numbers:

-- 12496 -> 14288 -> 15472 -> 14536 -> 14264 (-> 12496 -> ...)

-- Since this chain returns to its starting point, it is called an
-- amicable chain.

-- Find the smallest member of the longest amicable chain with no
-- element exceeding one million.

module Euler095 where

import Data.List
import Data.Ord

-- http://en.wikipedia.org/wiki/Divisor_function

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

divisorSum :: Int -> Int
divisorSum n = (+(-n)) . product . map (sum . scanl (*) 1) . group . factors $ n

minStartChain :: Int -> Int -> [Int]
minStartChain limit n = helper [n]
  where helper ls@(x:_)
          | ds < n || ds > limit = []
          | ds == n = ls
          | ds `elem` ls = []
          | otherwise = helper (ds:ls)
          where ds = divisorSum x

result095 = last $ maximumBy (comparing length) [ls | x <- [2..1000000], let ls = minStartChain 1000000 x, not (null ls)]
