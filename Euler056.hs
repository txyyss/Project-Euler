-- A googol (10^100) is a massive number: one followed by one-hundred
-- zeros; 100^100 is almost unimaginably large: one followed by
-- two-hundred zeros. Despite their size, the sum of the digits in
-- each number is only 1.

-- Considering natural numbers of the form, a^b, where a, b < 100,
-- what is the maximum digital sum?

module Euler056 where

import Data.Char (digitToInt)

sumDigits :: Integer -> Int
sumDigits = sum . map (fromIntegral . digitToInt) . show

maxDigitalSum :: Int -> Int -> Int
maxDigitalSum bMax a = helper a 1 $ fromIntegral a
  where helper :: Int -> Int -> Integer -> Int
        helper currMax b power
          | b == bMax = newMax
          | otherwise = helper newMax (b+1) (power * fromIntegral a)
          where newMax = max currMax $ sumDigits power

result056 = maximum $ map (maxDigitalSum 100) [1..100]
