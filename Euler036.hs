-- The decimal number, 585 = 1001001001 (binary), is palindromic in
-- both bases.

-- Find the sum of all numbers, less than one million, which are
-- palindromic in base 10 and base 2.

-- (Please note that the palindromic number, in either base, may not
-- include leading zeros.)

module Euler where

import Numeric (showIntAtBase)
import Data.Char (intToDigit)

binaryPresent :: Int -> String
binaryPresent n = showIntAtBase 2 intToDigit n ""

binaryPalindromic :: Int -> Bool
binaryPalindromic n = (reverse b) == b
  where b = binaryPresent n

normalPalindromic :: Int -> Bool
normalPalindromic n = (reverse o) == o
  where o = show n

result036 = sum $ filter (\x -> (binaryPalindromic x) && (normalPalindromic x)) [1..1000000]
