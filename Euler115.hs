-- NOTE: This is a more difficult version of problem 114.

-- A row measuring n units in length has red blocks with a minimum
-- length of m units placed on it, such that any two red blocks (which
-- are allowed to be different lengths) are separated by at least one
-- black square.

-- Let the fill-count function, F(m, n), represent the number of ways
-- that a row can be filled.

-- For example, F(3, 29) = 673135 and F(3, 30) = 1089155.

-- That is, for m = 3, it can be seen that n = 30 is the smallest
-- value for which the fill-count function first exceeds one million.

-- In the same way, for m = 10, it can be verified that F(10, 56) =
-- 880711 and F(10, 57) = 1148904, so n = 57 is the least value for
-- which the fill-count function first exceeds one million.

-- For m = 50, find the least value of n for which the fill-count
-- function first exceeds one million.

module Euler115 where

import Data.Array

countWays :: Integer -> Integer -> Integer
countWays m n = cache ! n
  where bnds = (-1,n)
        cache = listArray bnds $ map helper [-1..n]
        helper x
          | x < m = 1
          | otherwise = 1 + sum [cache ! (x-startPos-blockLen-1) | startPos <- [0..(x-m)], blockLen <- [m..(x-startPos)]]

result115 = head $ filter ((>1000000) . countWays 50) [1..]
