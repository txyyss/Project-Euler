-- It can be seen that the number, 125874, and its double, 251748,
-- contain exactly the same digits, but in a different order.

-- Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x,
-- and 6x, contain the same digits.

module Euler052 where

import Data.List (sort)

theSameDigits :: Int -> Int -> Bool
theSameDigits n m = sort (show n) == sort (show m)

isWanted :: Int -> Bool
isWanted n = all (theSameDigits n) $ map (*n) [2..6]

result052 = head $ filter isWanted [1..]
