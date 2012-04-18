-- Take the number 192 and multiply it by each of 1, 2, and 3:

-- 192  1 = 192
-- 192  2 = 384
-- 192  3 = 576

-- By concatenating each product we get the 1 to 9 pandigital,
-- 192384576. We will call 192384576 the concatenated product of 192
-- and (1,2,3)

-- The same can be achieved by starting with 9 and multiplying by 1,
-- 2, 3, 4, and 5, giving the pandigital, 918273645, which is the
-- concatenated product of 9 and (1,2,3,4,5).

-- What is the largest 1 to 9 pandigital 9-digit number that can be
-- formed as the concatenated product of an integer with (1,2, ... ,
-- n) where n > 1?

module Euler where

import Data.List (sort)
import Data.Char (digitToInt)

isPandigital :: Int -> (Int, Int)
isPandigital m = helper "" m 0
  where helper curr num n
          | len > 9 = (0, 0)
          | len < 9 = helper (curr ++ show (num * (n + 1))) num (n + 1)
          | sort curr == "123456789" = (num, n)
          | otherwise = (0, 0)
          where len = length curr

pandigitalValue :: (Int, Int) -> Int
pandigitalValue (0, 0) = 0
pandigitalValue (num, n) = foldl1 (\x y -> x * 10 + y) . map digitToInt. concatMap show $ map (* num) [1..n]

result038 = maximum $ map (pandigitalValue . isPandigital) [1..9999]
