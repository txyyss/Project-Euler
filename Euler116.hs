-- A row of five black square tiles is to have a number of its tiles
-- replaced with coloured oblong tiles chosen from red (length two),
-- green (length three), or blue (length four).

-- If red tiles are chosen there are exactly seven ways this can be
-- done.

-- If green tiles are chosen there are three ways.
 
-- And if blue tiles are chosen there are two ways.
	
-- Assuming that colours cannot be mixed there are 7 + 3 + 2 = 12 ways
-- of replacing the black tiles in a row measuring five units in
-- length.

-- How many different ways can the black tiles in a row measuring
-- fifty units in length be replaced if colours cannot be mixed and at
-- least one coloured tile must be used?

-- NOTE: This is related to problem 117.

module Euler116 where

import Data.Array

countWays :: Integer -> Integer -> Integer
countWays m n = cache ! n - 1
  where bnds = (0,n)
        cache = listArray bnds $ map helper [0..n]
        helper x
          | x < m = 1
          | otherwise = 1 + sum [cache ! (x-startPos-m) | startPos <- [0..(x-m)]]

result116 = sum $ map (`countWays` 50) [2..4]
