-- A row measuring seven units in length has red blocks with a minimum
-- length of three units placed on it, such that any two red blocks
-- (which are allowed to be different lengths) are separated by at
-- least one black square. There are exactly seventeen ways of doing
-- this.
 
-- How many ways can a row measuring fifty units in length be filled?

-- NOTE: Although the example above does not lend itself to the
-- possibility, in general it is permitted to mix block sizes. For
-- example, on a row measuring eight units in length you could use red
-- (3), black (1), and red (4).

module Euler114 where

import Data.Array

countWays :: Integer -> Integer
countWays m = cache ! m
  where bnds = (-1,m)
        cache = listArray bnds $ map helper [-1..m]
        helper x
          | x < 3 = 1
          | otherwise = 1 + sum [cache ! (x-startPos-blockLen-1) | startPos <- [0..(x-3)], blockLen <- [3..(x-startPos)]]

result114 = countWays 50
