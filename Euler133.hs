-- A number consisting entirely of ones is called a repunit. We shall
-- define R(k) to be a repunit of length k; for example, R(6) =
-- 111111.

-- Let us consider repunits of the form R(10ⁿ).

-- Although R(10), R(100), or R(1000) are not divisible by 17,
-- R(10000) is divisible by 17. Yet there is no value of n for which
-- R(10ⁿ) will divide by 19. In fact, it is remarkable that 11, 17,
-- 41, and 73 are the only four primes below one-hundred that can be a
-- factor of R(10ⁿ).

-- Find the sum of all the primes below one-hundred thousand that will
-- never be a factor of R(10ⁿ).

module Euler133 where

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

result133 = sum $ filter (\n -> modPow 10 (10^16) (9 * n) /= 1) $ takeWhile (<100000) primes
