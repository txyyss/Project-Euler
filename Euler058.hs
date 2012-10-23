-- Starting with 1 and spiralling anticlockwise in the following way,
-- a square spiral with side length 7 is formed.

-- 37 36 35 34 33 32 31
-- 38 17 16 15 14 13 30
-- 39 18  5  4  3 12 29
-- 40 19  6  1  2 11 28
-- 41 20  7  8  9 10 27
-- 42 21 22 23 24 25 26
-- 43 44 45 46 47 48 49

-- It is interesting to note that the odd squares lie along the bottom
-- right diagonal, but what is more interesting is that 8 out of the
-- 13 numbers lying along both diagonals are prime; that is, a ratio
-- of 8/13 62%.

-- If one complete new layer is wrapped around the spiral above, a
-- square spiral with side length 9 will be formed. If this process is
-- continued, what is the side length of the square spiral for which
-- the ratio of primes along both diagonals first falls below 10%?

module Euler058 where

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

cornerPrimesCount :: Int -> Int
cornerPrimesCount n = length . filter isPrime . take 3 $ iterate (\x->x-sub) start
  where start = n * n - sub
        sub = n - 1;

lengthBelow :: [Int] -> Int -> Int
lengthBelow ls n = helper ls 3 3 5
  where helper (x:xs) sum currLen diagCount
          | sum * n < diagCount = currLen
          | otherwise = helper xs (sum + cornerPrimesCount x) x (diagCount + 4)

result058 = lengthBelow [5,7..] 10
