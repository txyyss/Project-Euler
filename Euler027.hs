-- Euler published the remarkable quadratic formula:

-- n² + n + 41

-- It turns out that the formula will produce 40 primes for the
-- consecutive values n = 0 to 39. However, when n = 40, 40^2 + 40 +
-- 41 = 40(40 + 1) + 41 is divisible by 41, and certainly when n = 41,
-- 41² + 41 + 41 is clearly divisible by 41.

-- Using computers, the incredible formula n² - 79n + 1601 was
-- discovered, which produces 80 primes for the consecutive values n =
-- 0 to 79. The product of the coefficients, 79 and 1601, is 126479.

-- Considering quadratics of the form:

-- n² + an + b, where |a| < 1000 and |b| < 1000

-- where |n| is the modulus/absolute value of n
-- e.g. |11| = 11 and |4| = 4

-- Find the product of the coefficients, a and b, for the quadratic
-- expression that produces the maximum number of primes for
-- consecutive values of n, starting with n = 0.

module Euler027 where

import Data.List (maximumBy)
import Data.Ord (comparing)

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
isPrime n 
  | n <= 1 = False
  | otherwise = tryDivide primes
  where tryDivide (x:xs)
          | x * x > n = True
          | n `mod` x == 0 = False
          | otherwise = tryDivide xs
                        
primesTo :: Int -> [Int]
primesTo n = helper [] primes
  where helper existed (x:xs)
          | x > n = reverse existed
          | otherwise = helper (x:existed) xs

consecutivePrimeLength :: [Int] -> Int
consecutivePrimeLength = helper 0
  where helper curr [] = curr
        helper curr (y:ys)
          | isPrime y = helper (curr + 1) ys
          | otherwise = curr
                        
bCandidates = primesTo 1000

abCandidates = [(a,b) | b <- bCandidates, a <- [((-b - 4) `div` 2 + 1)..1000]]

lenOfPrimes :: (Int, Int) -> Int
lenOfPrimes x = consecutivePrimeLength $ map (quadratic x) [0..]
  where quadratic (a, b) n = n * (n + a) + b

result027 = uncurry (*) maxUnder1000
  where maxUnder1000 = fst $ maximumBy (comparing snd) $ zip abCandidates (map lenOfPrimes abCandidates)
