-- If we list all the natural numbers below 10 that are multiples of 3
-- or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

-- Find the sum of all the multiples of 3 or 5 below 1000.

module Euler001 where

-- naive version
result001 = sum [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]

-- more efficient version
sumDivisibleBy :: Integral a => a -> a -> a
sumDivisibleBy n upperLimit = n * p * (p+1) `div` 2
  where p = upperLimit `div` n

result001' = sumDivisibleBy 3 999 + sumDivisibleBy 5 999 - sumDivisibleBy 15 999
