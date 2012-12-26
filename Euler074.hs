-- The number 145 is well known for the property that the sum of the
-- factorial of its digits is equal to 145:

-- 1! + 4! + 5! = 1 + 24 + 120 = 145

-- Perhaps less well known is 169, in that it produces the longest
-- chain of numbers that link back to 169; it turns out that there are
-- only three such loops that exist:

-- 169->363601->1454->169
-- 871->45361->871
-- 872->45362->872

-- It is not difficult to prove that EVERY starting number will eventually get stuck in a loop. For example,

-- 69->363600->1454->169->363601->(->1454)
-- 78->45360->871->45361->(->871)
-- 540->145->(->145)

-- Starting with 69 produces a chain of five non-repeating terms, but
-- the longest non-repeating chain with a starting number below one
-- million is sixty terms.

-- How many chains, with a starting number below one million, contain
-- exactly sixty non-repeating terms?

module Euler074 where

import Data.Array
import Data.Char

factorial :: Array Int Int
factorial = listArray (0,9) (1:scanl1 (*) [1..9])

repeatCount :: Int -> Int
repeatCount n = helper [n] 1
  where next = sum . map ((factorial !) . digitToInt) . show
        helper ls@(x:_) len
          | nextX `elem` ls = len
          | otherwise = helper (nextX:ls) (len+1)
          where nextX = next x

result074 = length . filter (==60) $ map repeatCount [1..1000000]
