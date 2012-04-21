-- The first two consecutive numbers to have two distinct prime
-- factors are:

-- 14 = 2 * 7
-- 15 = 3 * 5

-- The first three consecutive numbers to have three distinct prime
-- factors are:

-- 644 = 2² * 7 * 23
-- 645 = 3 * 5 * 43
-- 646 = 2 * 17 * 19.

-- Find the first four consecutive integers to have four distinct
-- primes factors. What is the first of these numbers?

module Euler where

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

primesTo :: Int -> [Int]
primesTo n = helper [] $ primes
  where helper existed (x:xs)
          | x <= n = helper (x:existed) xs
          | otherwise = existed

findFactors :: Int -> [Int]
findFactors x = testDiv [] x $ primesTo (floor . sqrt $ fromIntegral x)
  where testDiv r 1 [] = r
        testDiv r y [] = (y:r)
        testDiv r target (y:ys)
          | target `mod` y == 0 = testDiv (y:r) (remove target y) ys
          | otherwise = testDiv r target ys
        remove m n
          | m `mod` n == 0 = remove (m `div` n) n
          | otherwise = m

findFirst 4 num _ = num - 3
findFirst n num (x:xs) 
  | x - num /= 1 = findFirst 1 x xs
  | otherwise = findFirst (n+1) x xs

result047 = findFirst 0 0 $ filter (\x -> length (findFactors x) == 4) [2..]
