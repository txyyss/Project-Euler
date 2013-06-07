-- A number consisting entirely of ones is called a repunit. We shall
-- define R(k) to be a repunit of length k.

-- For example, R(10) = 1111111111 = 11 × 41 × 271 × 9091, and the sum
-- of these prime factors is 9414.

-- Find the sum of the first forty prime factors of R(10⁹).

module Euler132 where

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

modPow :: Integer -> Integer -> Integer -> Integer
modPow a b m = helper a b
  where helper a 1 = a `mod` m
        helper a b
          | even b  = half * half `mod` m
          | otherwise = a * half * half `mod` m
          where half = helper a (b `div` 2)

limitedFacOfRepunit k limit = helper [] primes
  where helper ls (p:ps)
          | length ls == limit = ls
          | modPow 10 k (9 * p) == 1 = helper (p:ls) ps
          | otherwise = helper ls ps

result132 = sum $ limitedFacOfRepunit (10^9) 40
