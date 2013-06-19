-- Modified Fibonacci golden nuggets

module Euler140 where

-- Analysis:
--   A_G(x) = x G_1 + x^2 G_2 + x^3 G_3 + x^4 G_4 + ...
-- x A_G(x) =         x^2 G_1 + x^3 G_2 + x^4 G_3 + ...
-- (1 - x) A_G(x) = x + 3 * x^2 + x^2 A_G(x)
-- A_G(x) = x * (1 + 3 * x) / (1 - x - x^2)
-- Solve A_G(x) = n, we need 5n^2 + 14n + 1 to be a perfect square.
-- 5n^2 + 14n + 1 = m^2 is another quadratic Diophantine equation.
-- This equation has solution only when k^2 - 5m^2 = 44, Pell's Equation.

-- (k,m) pairs, k = 5m^2+44

import Data.List(sort)

fundamentalSols = [(7,1),(8,2),(13,5),(17,7),(32,14),(43,19)]

genSols limit = map (\(k,_)->(k-7) `div` 5) . take limit . filter (\(x,_)->(x-7) `mod` 5 == 0) .
                iterate (\(a,b)->(9 * a + 20 * b, 4 * a + 9 * b))

result140 = sum . take 31 . sort $ concatMap (genSols 30) fundamentalSols
