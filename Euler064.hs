-- All square roots are periodic when written as continued fractions.

-- For conciseness, we use the notation √23 = [4;(1,3,1,8)], to
-- indicate that the block (1,3,1,8) repeats indefinitely.

-- The first ten continued fraction representations of (irrational)
-- square roots are:

-- √2=[1;(2)], period=1
-- √3=[1;(1,2)], period=2
-- √5=[2;(4)], period=1
-- √6=[2;(2,4)], period=2
-- √7=[2;(1,1,1,4)], period=4
-- √8=[2;(1,4)], period=2
-- √10=[3;(6)], period=1
-- √11=[3;(3,6)], period=2
-- √12= [3;(2,6)], period=2
-- √13=[3;(1,1,1,1,6)], period=5

-- Exactly four continued fractions, for N <= 13, have an odd period.

-- How many continued fractions for N <= 10000 have an odd period?

module Euler064 where

import Data.List

nextTuple :: (Int, Int, Int, Int, Int) -> (Int, Int, Int, Int, Int)
nextTuple (a,b,c,d,e) = (newA, tB `div` comD, c, tD `div` comD, tE `div` comD)
  where newA = floor $ (fromIntegral tB * (sqrt $ fromIntegral c) - fromIntegral (e * d)) / (fromIntegral tE)
        tB = e * b
        tD = (-e) * d - newA * tE
        tE = b * b * c - d * d
        comD = gcd tD $ gcd tB tE

findRepeat :: Int -> Int
findRepeat n = helper [(intRoot, 1, n, (-intRoot), 1)]
  where intRoot = floor . sqrt $ fromIntegral n
        helper ls@(x:_) = addOrReturn $ elemIndex newX ls
          where newX = nextTuple x
                addOrReturn Nothing = helper (newX:ls)
                addOrReturn (Just m) = m + 1

result064 = length . filter odd . map findRepeat $ [1..10000] \\ (map (^2) [1..100])
