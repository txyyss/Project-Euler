-- The number 512 is interesting because it is equal to the sum of its
-- digits raised to some power: 5 + 1 + 2 = 8, and 83 = 512. Another
-- example of a number with this property is 614656 = 284.

-- We shall define an to be the nth term of this sequence and insist
-- that a number must contain at least two digits to have a sum.

-- You are given that a2 = 512 and a10 = 614656.

-- Find a30.

module Euler119 where

import Data.Char
import Data.List

digitSum :: Integer -> Int
digitSum = sum . map digitToInt . show

result119 = last . take 30 $ sort [num | b <- [2..400], e <- [2..50],let num = b^e, digitSum num == fromIntegral b]
