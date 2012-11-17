-- A natural number, N, that can be written as the sum and product of
-- a given set of at least two natural numbers, {a1, a2, ... , ak} is
-- called a product-sum number: N = a1 + a2 + ... + ak = a1 x a2 x
-- ... x ak.

-- For example, 6 = 1 + 2 + 3 = 1 x 2 x 3.

-- For a given set of size, k, we shall call the smallest N with this
-- property a minimal product-sum number. The minimal product-sum
-- numbers for sets of size, k = 2, 3, 4, 5, and 6 are as follows.

-- k=2: 4 = 2 x 2 = 2 + 2
-- k=3: 6 = 1 x 2 x 3 = 1 + 2 + 3
-- k=4: 8 = 1 x 1 x 2 x 4 = 1 + 1 + 2 + 4
-- k=5: 8 = 1 x 1 x 2 x 2 x 2 = 1 + 1 + 2 + 2 + 2
-- k=6: 12 = 1 x 1 x 1 x 1 x 2 x 6 = 1 + 1 + 1 + 1 + 2 + 6

-- Hence for 2<=k<=6, the sum of all the minimal product-sum numbers
-- is 4+6+8+12 = 30; note that 8 is only counted once in the sum.

-- In fact, as the complete set of minimal product-sum numbers for
-- 2<=k<=12 is {4, 6, 8, 12, 15, 16}, the sum is 61.

-- What is the sum of all the minimal product-sum numbers for
-- 2<=k<=12000?

module Euler088 where

import Data.Array
import Data.List

-- hint by
-- http://www.mathblog.dk/project-euler-88-minimal-product-sum-numbers/

genKTuple :: Int -> [(Int,Int)]
genKTuple maxProd = concatMap (\n->helper [n] n n 1) [2..ceiling $ sqrt $ fromIntegral maxProd]
  where helper ls@(x:_) prod sm len = curResult ++ result
          where newRange = [x..maxProd `div` prod]
                curResult = map (\n-> (prod * n - sm - n + len + 1, prod * n)) newRange
                result = concatMap (\n -> helper (n:ls) (n*prod) (n+sm) (len+1)) newRange

minimalProductSumNum :: Array Int Int
minimalProductSumNum = accumArray min 25000 (2,24000) $ genKTuple 24000

result088 = sum $ map head $ group $ sort $ map (minimalProductSumNum!) [2..12000]
