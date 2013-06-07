-- A number consisting entirely of ones is called a repunit. We shall
-- define R(k) to be a repunit of length k; for example, R(6) =
-- 111111.

-- Given that n is a positive integer and GCD(n, 10) = 1, it can be
-- shown that there always exists a value, k, for which R(k) is
-- divisible by n, and let A(n) be the least such value of k; for
-- example, A(7) = 6 and A(41) = 5.

-- You are given that for all primes, p > 5, that p - 1 is divisible
-- by A(p). For example, when p = 41, A(41) = 5, and 40 is divisible
-- by 5.

-- However, there are rare composite values for which this is also
-- true; the first five examples being 91, 259, 451, 481, and 703.

-- Find the sum of the first twenty-five composite values of n for
-- which GCD(n, 10) = 1 and n - 1 is divisible by A(n).

module Euler130 where

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

composites = helper [4..] primes
  where helper (x:xs) pl@(p:ps)
          | x == p = helper xs ps
          | x < p = x : helper xs pl
          | x > p = x : helper xs ps

calA n = (+1) . length . takeWhile (/=0) $ iterate (\x -> (x * 10 + 1) `mod` n) 1

result130 = sum . take 25 . filter (\n -> (n - 1) `mod` calA n == 0) $ filter ((==1) . gcd 10) composites
