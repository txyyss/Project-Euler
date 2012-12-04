-- Using a combination of black square tiles and oblong tiles chosen
-- from: red tiles measuring two units, green tiles measuring three
-- units, and blue tiles measuring four units, it is possible to tile
-- a row measuring five units in length in exactly fifteen different
-- ways.
 
-- How many ways can a row measuring fifty units in length be tiled?

-- NOTE: This is related to problem 116.

module Euler117 where

import Data.Array

countWays :: Integer -> Integer
countWays n = cache ! n
  where bnds = (0,n)
        cache = listArray bnds $ map helper [0..n]
        helper x
          | x < 2 = 1
          | otherwise = 1 + sum [cache ! (x-startPos-blockLen) | blockLen <- [2..4], startPos <- [0..(x-blockLen)]]

result117 = countWays 50
