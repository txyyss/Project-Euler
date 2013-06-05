-- The palindromic number 595 is interesting because it can be written
-- as the sum of consecutive squares: 6² + 7² + 8² + 9² + 10² + 11² +
-- 12².

-- There are exactly eleven palindromes below one-thousand that can be
-- written as consecutive square sums, and the sum of these
-- palindromes is 4164. Note that 1 = 0² + 1² has not been included as
-- this problem is concerned with the squares of positive integers.

-- Find the sum of all the numbers less than 10⁸ that are both
-- palindromic and can be written as the sum of consecutive squares.

module Euler125 where

import Data.List
import Data.Set (fromList, fold)

isPalindrome :: Show a => a -> Bool
isPalindrome x = ori == rev
  where ori = show x
        rev = reverse ori
 
sumsFrom :: Int -> Int -> [Int]
sumsFrom hi i = takeWhile (<hi) . drop 1 . scanl1 (+) $ map (^2) [i..]
 
result125 = fold (+) 0 . fromList . concatMap (filter isPalindrome . sumsFrom (10^8)) $ [1 .. limit]
  where limit = floor . sqrt . fromIntegral $ (10^8 `div` 2)
