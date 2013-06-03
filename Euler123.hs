-- Let pₙ be the nth prime: 2, 3, 5, 7, 11, ..., and let r be the
-- remainder when (pₙ-1)ⁿ + (pₙ+1)ⁿ is divided by pₙ².

-- For example, when n = 3, p₃ = 5, and 4³ + 6³ = 280 = 5 mod 25.

-- The least value of n for which the remainder first exceeds 10⁹ is
-- 7037.

-- Find the least value of n for which the remainder first exceeds
-- 10¹⁰.

module Euler123 where

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

modPow :: Integer -> Integer -> Integer -> Integer
modPow a b m = helper a b
  where helper a 1 = a `mod` m
        helper a b
          | even b  = half * half `mod` m
          | otherwise = a * half * half `mod` m
          where half = helper a (b `div` 2)

specialRemainder :: Integer -> Integer -> Integer
specialRemainder pn n = (pow1 + pow2) `mod` pn2
  where pn2 = pn * pn
        pow1 = modPow (pn - 1) n pn2
        pow2 = modPow (pn + 1) n pn2

result123 = snd . head . filter (\(x, y) -> specialRemainder x y > 10^10) $ zip primes [1..]
