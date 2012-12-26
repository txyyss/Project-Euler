-- The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.

-- Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... +
-- 1000^1000.

module Euler048 where

result048 = sum [i^i | i <- [1..1000]] `mod` 10^10
