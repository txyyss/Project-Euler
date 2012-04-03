-- The sum of the squares of the first ten natural numbers is,

-- 1^2 + 2^2 + ... + 10^2 = 385 

--The square of the sum of the first ten natural numbers is,

-- (1 + 2 + ... + 10)^2 = 55^2 = 3025

-- Hence the difference between the sum of the squares of the first
-- ten natural numbers and the square of the sum is 3025 - 385 = 2640.

-- Find the difference between the sum of the squares of the first one
-- hundred natural numbers and the square of the sum.

module Euler where

sumSquare :: Integral a => a -> a
sumSquare n = n * (1 + n) * (1 + 2 * n) `div` 6

squareSum :: Integral a => a -> a
squareSum n = n^2 * (n+1)^2 `div` 4

result006 = squareSum 100 - sumSquare 100
