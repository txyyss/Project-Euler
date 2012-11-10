-- It is well known that if the square root of a natural number is not
-- an integer, then it is irrational. The decimal expansion of such
-- square roots is infinite without any repeating pattern at all.

-- The square root of two is 1.41421356237309504880..., and the
-- digital sum of the first one hundred decimal digits is 475.

-- For the first one hundred natural numbers, find the total of the
-- digital sums of the first one hundred decimal digits for all the
-- irrational square roots.

module Euler080 where

import Data.List
import Data.Char

intSqrt :: Integer -> Integer
intSqrt n = fst . head . dropWhile (\(a,b) -> a + 1 /= b && a /= b) $ iterate (\(_,x) -> (x, (x + div n x) `div` 2)) (2, 1)

sumSqrt100 :: Integer -> Int
sumSqrt100 n = sum . map digitToInt . take 100 $ show digits
  where digits = intSqrt (n * 10^206)

result080 = sum . map sumSqrt100 $ [1..100] \\ map (^2) [1..10]
