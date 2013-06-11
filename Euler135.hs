-- Given the positive integers, x, y, and z, are consecutive terms of
-- an arithmetic progression, the least value of the positive integer,
-- n, for which the equation, x² - y² - z² = n, has exactly two
-- solutions is n = 27:

-- 34² - 27² - 20² = 12² - 9² - 6² = 27

-- It turns out that n = 1155 is the least value which has exactly ten
-- solutions.

-- How many values of n less than one million have exactly ten
-- distinct solutions?

module Euler135 where

-- (x + k)² - x² - (x - k)² = n
-- x (4 k - x) = n
-- x > k && 4 k - x > 0
-- so x / 4 < k < x

import Data.Array

genArr :: Int -> Array Int Int
genArr limit = accumArray (+) 0 (1, limit) [(x * y, 1) |
                                            x <- [1..limit], y <- [1..(limit `div` x)],
                                            (x + y) `mod` 4 == 0 ,
                                            let k = (x + y) `div` 4, k < x]

result135 = length $ filter (==10) $ elems $ genArr (10^6)
