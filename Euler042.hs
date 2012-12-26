-- The nth term of the sequence of triangle numbers is given by, tn =
-- n(n+1)/2; so the first ten triangle numbers are:

-- 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

-- By converting each letter in a word to a number corresponding to
-- its alphabetical position and adding these values we form a word
-- value. For example, the word value for SKY is 19 + 11 + 25 = 55 =
-- t10. If the word value is a triangle number then we shall call the
-- word a triangle word.

-- Using words.txt (right click and 'Save Link/Target As...'), a 16K
-- text file containing nearly two-thousand common English words, how
-- many are triangle words?

module Euler042 where

import Data.Char (ord, toUpper)

split :: String -> [String]
split = foldr splitHelper [[]]
  where splitHelper ',' existed = []:existed
        splitHelper '"' existed = existed
        splitHelper curr (x:xs) = (curr:x) : xs

getScore :: String -> Int
getScore = sum . map alphaValue
  where alphaValue x = ord (toUpper x) - ord 'A' + 1

isTriangular :: Int -> Bool
isTriangular x = isPerfect (8 * x + 1)
  where isPerfect m = r * r == m
          where r = floor . sqrt $ fromIntegral m

countTriangulars :: [String] -> Int
countTriangulars = length . filter isTriangular . map getScore

result042 = do
  content <- readFile "data/words.txt"
  return . countTriangulars $ split content
