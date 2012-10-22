-- A Pythagorean triplet is a set of three natural numbers, a  b  c, for which,

-- a^2 + b^2 = c^2

-- For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.

-- Find the product abc.

module Euler009 where

-- All Pythagorean triples can be represented as (m^2 - n^2, 2*m*n,
-- m^2 + n^2) with m > n. The sum of triple is 2 * m * (m + n).

mAndN = [(m,n) | n <- [1..32], m <- [(n+1)..32], m * (m+n)==500]

result009 = (m^2 - n^2) * (2 * m * n) * (m^2 + n^2)
  where (m,n) = head mAndN
