-- There are exactly ten ways of selecting three from five, 12345:

-- 123, 124, 125, 134, 135, 145, 234, 235, 245, and 345

-- In combinatorics, we use the notation, 5C3 = 10.

-- In general,

-- nCr = n! / r!(nr)!

-- where r <= n, n! = n(n1)...321, and 0! = 1.  It is not until n =
-- 23, that a value exceeds one-million: 23C10 = 1144066.

-- How many, not necessarily distinct, values of nCr, for 1 <= n <=
-- 100, are greater than one-million?

module Euler053 where

binomial :: Integral a => a -> a -> a
binomial n r = product [(n-r+1)..n] `div` product [2..r]

exceed :: Integral a => a -> Int
exceed n = length . filter (>1000000) $ map (binomial n) [0..n]

result053 = sum $ map exceed [1..100]
