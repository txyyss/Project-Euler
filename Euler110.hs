-- In the following equation x, y, and n are positive integers.

-- 1/x + 1/y = 1/n

-- It can be verified that when n = 1260 there are 113 distinct
-- solutions and this is the least value of n for which the total
-- number of distinct solutions exceeds one hundred.

-- What is the least value of n for which the number of distinct
-- solutions exceeds four million?

-- NOTE: This problem is a much more difficult version of problem 108
-- and as it is well beyond the limitations of a brute force approach
-- it requires a clever implementation.

module Euler110 where

-- see first observation in
-- http://www.mathblog.dk/project-euler-110-efficient-diophantine-equation/

import Data.List

minus :: Ord a => [a] -> [a] -> [a]
minus lx@(x:xs) ly@(y:ys) = case compare x y of 
  LT -> x : minus xs ly
  EQ ->     minus xs ys 
  GT ->     minus lx ys
minus xs _ = xs

primes :: [Integer]
primes = 2 : primes'
  where
    primes' = sieve [3,5..] 9 primes'
    sieve (x:xs) q ps@ ~(p:t)
      | x < q     = x : sieve xs q ps
      | otherwise =     sieve (xs `minus` [q, q+2*p..]) (head t^2) t

series :: [Integer] -> Int -> [[Integer]]
series _ 0 =[[]]
series xs n =[x:ps | x <- xs, ps <- series [0..x] (n-1)]

leastN :: Int -> Integer -> Integer
leastN count limit = minimum . map (product . zipWith (^) (take count primes)) $
                     filter ((>limit) . (`div` 2) . (+1) . product . map ((+1).(*2))) $ series [1..3] count

result110 = leastN 15 4000000
