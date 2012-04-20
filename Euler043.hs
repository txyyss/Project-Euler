-- The number, 1406357289, is a 0 to 9 pandigital number because it is
-- made up of each of the digits 0 to 9 in some order, but it also has
-- a rather interesting sub-string divisibility property.

-- Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this
-- way, we note the following:

-- d2d3d4=406 is divisible by 2
-- d3d4d5=063 is divisible by 3
-- d4d5d6=635 is divisible by 5
-- d5d6d7=357 is divisible by 7
-- d6d7d8=572 is divisible by 11
-- d7d8d9=728 is divisible by 13
-- d8d9d10=289 is divisible by 17

-- Find the sum of all 0 to 9 pandigital numbers with this property.

module Euler where

import Data.List (permutations)

subNum :: [Integer] -> [Integer]
subNum l = map (foldr1 (\x y -> x + 10 * y)) r
  where r = cut [] [] [] l
        cut a [] [] (x:xs) = cut a [x] [] xs
        cut a b [] (x:xs) = cut a (x:b) [x] xs
        cut a b c (x:xs) = cut ((x:b):a) (x:c) [x] xs
        cut a _ _ [] = a

isWanted :: [Integer] -> Bool
isWanted = and . zipWith (\x y -> y `mod` x == 0) [17, 13, 11, 7, 5, 3, 2] . subNum

result043 = sum . map (foldl1 (\x y -> x * 10 + y)) . filter isWanted $ permutations [0..9]
