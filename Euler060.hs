-- The primes 3, 7, 109, and 673, are quite remarkable. By taking any
-- two primes and concatenating them in any order the result will
-- always be prime. For example, taking 7 and 109, both 7109 and 1097
-- are prime. The sum of these four primes, 792, represents the lowest
-- sum for a set of four primes with this property.

-- Find the lowest sum for a set of five primes for which any two
-- primes concatenate to produce another prime.

module Euler060 where

import Data.Ord
import Data.Maybe
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

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = helper primes
  where helper (x:xs)
          | x * x > n = True
          | n `mod` x == 0 = False
          | otherwise = helper xs

isPrimePair :: (Show m, Show n) => m -> n -> Bool
isPrimePair a b = isPrime ab && isPrime ba
  where ab = con a b
        ba = con b a
        con x y = read . shows x $ show y

tryGet5 n = head
            [[a,b,c,d,e]|
             let pa = takeWhile (<=n) primes,
             a <- pa,
             let pb = filter (isPrimePair a) $ dropWhile (<=a) pa,
             b <- pb,
             let pc = filter (isPrimePair b) $ dropWhile (<=b) pb,
             c <- pc,
             let pd = filter (isPrimePair c) $ dropWhile (<=c) pc,
             d <- pd,
             let pe = filter (isPrimePair d) $ dropWhile (<=d) pd,
             e <- pe
             ]

result060 = sum $ tryGet5 10000
