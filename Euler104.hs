-- The Fibonacci sequence is defined by the recurrence relation:

-- F(n) = F(n-1) + F(n-2), where F(1) = 1 and F(2) = 1.

-- It turns out that F_541, which contains 113 digits, is the first
-- Fibonacci number for which the last nine digits are 1-9 pandigital
-- (contain all the digits 1 to 9, but not necessarily in order). And
-- F_2749, which contains 575 digits, is the first Fibonacci number
-- for which the first nine digits are 1-9 pandigital.

-- Given that F_k is the first Fibonacci number for which the first
-- nine digits AND the last nine digits are 1-9 pandigital, find k.

module Euler104 where

import Data.List

fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

isBothPandigital :: Integer -> Bool
isBothPandigital n = last9 == "123456789" && first9 == "123456789"
  where nRest = n `mod` 1000000000
        last9 = sort (show nRest)
        first9 = sort $ take 9 $ show n

result104 = snd $ head $ filter (isBothPandigital . fst) (zip fibs [1..])
