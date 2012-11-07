-- It is possible to write ten as the sum of primes in exactly five
-- different ways:

-- 7 + 3
-- 5 + 5
-- 5 + 3 + 2
-- 3 + 3 + 2 + 2
-- 2 + 2 + 2 + 2 + 2

-- What is the first value which can be written as the sum of primes
-- in over five thousand different ways?

module Euler077 where

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

primePartitionCount :: Int -> Int
primePartitionCount = helper primes
  where helper _ 0 = 1
        helper ls@(x:xs) m
          | m < x = 0
          | otherwise = helper ls (m-x) + helper xs m

result077 = fst . head . dropWhile ((<=5000) . snd) . zip [1..] $ map primePartitionCount [1..]
