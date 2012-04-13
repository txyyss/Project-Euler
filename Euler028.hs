-- Starting with the number 1 and moving to the right in a clockwise
-- direction a 5 by 5 spiral is formed as follows:

-- 21 22 23 24 25
-- 20  7  8  9 10
-- 19  6  1  2 11
-- 18  5  4  3 12
-- 17 16 15 14 13

-- It can be verified that the sum of the numbers on the diagonals is
-- 101.

-- What is the sum of the numbers on the diagonals in a 1001 by 1001
-- spiral formed in the same way?

module Euler where

sum4 :: Int -> Int
sum4 n = n * (4 * n - 6) + 6

diagonalsSum :: Int -> Int
diagonalsSum n = helper 0 n
  where helper curr 1 = curr + 1
        helper curr m = helper (curr + sum4 m) (m-2)

result028 = diagonalsSum 1001

-- improved fomula by Mathematica

diagonalsSum' :: Int -> Int
diagonalsSum' n = (-9 + n * (8 + n * (3 + 4 * n))) `div` 6

result028' = diagonalsSum' 1001
