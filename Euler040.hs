-- An irrational decimal fraction is created by concatenating the
-- positive integers:

-- 0.123456789101112131415161718192021...

-- It can be seen that the 12th digit of the fractional part is 1.

-- If dn represents the nth digit of the fractional part, find the
-- value of the following expression.

-- d1 * d10 * d100 * d1000 * d10000 * d100000 * d1000000

module Euler where

import Data.Char (digitToInt)

digitCount = scanl1 (+) . zipWith (*) [1..] . take 7 $ iterate (* 10) 9

findPos :: Int -> Int
findPos n = helper 0
  where helper p
          | n <= digitCount !! p = p
          | otherwise = helper (p + 1)

rawPos :: Int -> (Int, Int)
rawPos n
  | pos == 0 = (1, n)
  | otherwise = (pos + 1, n - (digitCount !! (pos - 1)))
  where pos = findPos n

digitAt :: Int -> Int
digitAt n = digitToInt $ (show num) !! ((p-1) `mod` w) 
  where num = 10^(w-1) + (p - 1) `div` w
        (w, p) = rawPos n

result040 = product . map digitAt . take 7 $ iterate (* 10) 1
