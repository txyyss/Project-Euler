-- It was proposed by Christian Goldbach that every odd composite
-- number can be written as the sum of a prime and twice a square.

-- 9 = 7 + 2 * 1^2
-- 15 = 7 + 2 * 2^2
-- 21 = 3 + 2 * 3^2
-- 25 = 7 + 2 * 3^2
-- 27 = 19 + 2 * 2^2
-- 33 = 31 + 2 * 1^2

-- It turns out that the conjecture was false.

-- What is the smallest odd composite that cannot be written as the
-- sum of a prime and twice a square?

module Euler046 where

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

isPerfect m = r * r == m
  where r = floor . sqrt $ fromIntegral m
        
oddPrimesTo n = helper [] $ tail primes
  where helper existed (x:xs)
          | x <= n = helper (x:existed) xs
          | otherwise = existed

canPass n
  | isPrime n = True
  | otherwise = or . map (\m -> isPerfect ((n-m) `div` 2)) $ oddPrimesTo n

result046 = head $ filter (not . canPass) [3,5..]
