-- The following iterative sequence is defined for the set of positive
-- integers:

-- n -> n/2 (n is even)
-- n -> 3n + 1 (n is odd)

-- Using the rule above and starting with 13, we generate the
-- following sequence:

-- 13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1

-- It can be seen that this sequence (starting at 13 and finishing at
-- 1) contains 10 terms. Although it has not been proved yet (Collatz
-- Problem), it is thought that all starting numbers finish at 1.

-- Which starting number, under one million, produces the longest
-- chain?

-- NOTE: Once the chain starts the terms are allowed to go above one
-- million.

module Euler where

import Data.Array
import Data.List (maximumBy)
import Data.Ord (comparing)

collatzSizeListTo :: (Ix i, Num e, Integral i) => i -> Array i e
collatzSizeListTo n = a
  where a = listArray (1,n) $ 1:[collatzSize x | x <- [2..n]]
        collatzSize m
          | m' <= n = 1 + a ! m'
          | otherwise = 1 + collatzSize m'
          where m' = if even m then m `div` 2 else 3 * m + 1

result014 = fst $ maximumBy (comparing snd) $ assocs $ collatzSizeListTo 1000000
