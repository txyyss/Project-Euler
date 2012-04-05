-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

-- Find the sum of all the primes below two million.

module Euler where

minus :: Ord a => [a] -> [a] -> [a]
minus lx@(x:xs) ly@(y:ys) = case (compare x y) of 
  LT -> x : minus xs ly
  EQ ->     minus xs ys 
  GT ->     minus lx ys
minus xs _ = xs

primesTo :: Integral a => a -> [a]
primesTo m = 2 : sieve [3,5..m]  where
  sieve (p:xs) 
    | p*p > m   = p : xs
    | otherwise = p : sieve (xs `minus` [p*p, p*p+2*p..])
                  
result010 = sum $ primesTo 2000000
