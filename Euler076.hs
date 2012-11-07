-- It is possible to write five as a sum in exactly six different
-- ways:

-- 4 + 1
-- 3 + 2
-- 3 + 1 + 1
-- 2 + 2 + 1
-- 2 + 1 + 1 + 1
-- 1 + 1 + 1 + 1 + 1

-- How many different ways can one hundred be written as a sum of at
-- least two positive integers?

module Euler076 where

import Data.Array

partitionArray :: Int -> Array (Int,Int) Int
partitionArray limit = result
  where result = listArray ((1,1),(limit,limit)) [helper x y | x<-[1..limit], y<-[1..limit]]
        helper 1 _ = 1
        helper _ 1 = 1
        helper n m
          | n < m = result ! (n,n)
          | m == n = 1 + result ! (n,n-1)
          | otherwise = sub1 + sub2
          where sub1 = result ! (n-m,m)
                sub2 = result ! (n,m-1)

result076 = m ! (100,99)
  where m = partitionArray 100
