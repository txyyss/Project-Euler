-- The positive integers, x, y, and z, are consecutive terms of an
-- arithmetic progression. Given that n is a positive integer, the
-- equation, x² - y² - z² = n, has exactly one solution when n = 20:

-- 13² - 10² - 7² = 20

-- In fact there are twenty-five values of n below one hundred for
-- which the equation has a unique solution.

-- How many values of n less than fifty million have exactly one
-- solution?

module Euler136 where

-- (x + k)² - x² - (x - k)² = n
-- x (4 k - x) = n
-- x > k && 4 k - x > 0
-- so x / 4 < k <= x

-- we need to find which n has single solution.
-- Represent n as n = 2ʳs, s is the odd factor of n

-- case 1: r = 0 and s is a prime, or say n is an odd prime
-- one solution: x = 1 and 4 k - x = n, k must be 1. n = 3. n > 3, no.
-- another solution: x = n and 4k - x = 1, n = 4k + 3, k >= 1, OK.
-- We get: if n is an odd prime and n mod 4 = 3, single solution.

-- case 2: r = 0 and s (=n) is not a prime but an odd factor.
-- suppose that n = p q and p <= q. Both p q are odd integers.
-- represent p = 2 i + 1, q = 2 j + 1, or j = i + a, a > 0
-- for x (4 k - x) = p q, k < x, we have 3 possible solutions:
-- solution 1: 
-- x = p q and 4 k - x = 1, or say 4 k - (2i+1)(2(i+a)+1) = 1
-- we can get k = (a + 1) / 2 + i + a i + i².
-- It can be checked that k < x always holds.
-- So if a is odd, we have one solution.
-- solution 2:
-- x = p and 4 k - x = q, or say 4 k - (2i+1) = 2(i+a) + 1
-- we can get k = (a + 1) / 2 + i.
-- for k < x, we need a < 2 i + 1
-- So if a is odd and a < 2 i + 1, we have one solution.
-- solution 3:
-- x = q and 4 k - x = p, or say 4 k - (2(i+a)+1) = 2 i + 1
-- we can get k = (a + 1) / 2 + i.
-- It can be checked that k < x always holds.
-- So if a is odd, we have one solution.
-- Conclusion: If a is even, 0 solution.
-- If a is odd, 2 or 3 solutions.

-- case 3: r = 1 and s is a odd
-- Equation is x (4 k - x) = 2 s
-- x = 2 is impossible since k < x
-- For x = s and 4 k - x = 2, we get k = (2 + s) / 4.
-- k can't be an integer. So no solutions.

-- Similarly we can analyse case 4: r = 2 and s is prime
-- Conclusion: that n = 4 is a solution and so is every
-- case where n = 4s when s is a prime

-- case 4: r = 2 and s is not prime
-- Conclusion: multiple solutions

-- case 5: r = 3 and s is odd
-- Conclusion: no solutions

-- case 6: r = 4 and s is an odd prime.
-- Conclusion: n = 16 is a solution and so is
-- every case where n = 16s when s is a prime

-- case 7: r = 4 and s is not a prime
-- Conclusion: Either 0 solution or multiple solutions

-- case 8: r > 4 and s is odd
-- Conclusion: multiple solutions

-- Put all together
-- One solution:
-- n = 4p, where p is an odd prime
-- n = 16p, where p is an odd prime
-- n = p, where p is prime and p = 4k+3
-- otherwise, no solution or multiple solutions.

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

countSingleSolution n = helper 0 primes
  where helper i (p:ps)
          | p > n = i
          | otherwise = helper (i + a + b + c) ps
          where a = if p < nd4 then 1 else 0
                b = if p < nd16 then 1 else 0
                c = if p `mod` 4 == 3 then 1 else 0
        nd4 = n `div` 4
        nd16 = n `div` 16

result136 = countSingleSolution (5 * 10^7)
