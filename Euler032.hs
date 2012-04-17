-- We shall say that an n-digit number is pandigital if it makes use
-- of all the digits 1 to n exactly once; for example, the 5-digit
-- number, 15234, is 1 through 5 pandigital.

-- The product 7254 is unusual, as the identity, 39 * 186 = 7254,
-- containing multiplicand, multiplier, and product is 1 through 9
-- pandigital.

-- Find the sum of all products whose multiplicand/multiplier/product
-- identity can be written as a 1 through 9 pandigital.

-- HINT: Some products can be obtained in more than one way so be sure
-- to only include it once in your sum.

module Euler where

import Data.List (sort, group)

isPandigital :: Int -> Int -> Bool
isPandigital x y = (=="123456789") . sort $ show x ++ show y ++ show (x * y)

result032 = sum . map head . group $ sort [x*y | x <- [1..99], y <- [100..(9999 `div` x)], isPandigital x y]
