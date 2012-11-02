-- Euler's Totient function, φ(n) [sometimes called the phi function],
-- is used to determine the number of numbers less than n which are
-- relatively prime to n. For example, as 1, 2, 4, 5, 7, and 8, are
-- all less than nine and relatively prime to nine, φ(9)=6.

-- n	Relatively Prime	φ(n)	n/φ(n)
-- 2	1	1	2
-- 3	1,2	2	1.5
-- 4	1,3	2	2
-- 5	1,2,3,4	4	1.25
-- 6	1,5	2	3
-- 7	1,2,3,4,5,6	6	1.1666...
-- 8	1,3,5,7	4	2
-- 9	1,2,4,5,7,8	6	1.5
-- 10	1,3,7,9	4	2.5

-- It can be seen that n=6 produces a maximum n/φ(n) for n <= 10.

-- Find the value of n <= 1,000,000 for which n/φ(n) is a maximum.

module Euler069 where

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

findMax :: Int -> Int
findMax n = helper 1 primes
  where helper m (x:xs)
          | prod > n = m
          | otherwise = helper prod xs
          where prod = m * x

result069 = findMax 1000000
