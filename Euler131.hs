-- There are some prime values, p, for which there exists a positive
-- integer, n, such that the expression n³ + n²p is a perfect cube.

-- For example, when p = 19, 8³ + 8² × 19 = 12³.

-- What is perhaps most surprising is that for each prime with this
-- property the value of n is unique, and there are only four such
-- primes below one-hundred.

-- How many primes below one million have this remarkable property?

module Euler131 where

-- analysis can be found in
-- http://www.mathblog.dk/project-euler-131-primes-perfect-cube/

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

isPrime 1 = False
isPrime n = helper primes
  where helper (x:xs)
          | x * x > n = True
          | n `mod` x == 0 = False
          | otherwise = helper xs

result131 = length . filter isPrime . takeWhile (<1000000) $ [3 * i * (i + 1) + 1 | i <- [1..]]
