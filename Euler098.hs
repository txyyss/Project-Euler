-- By replacing each of the letters in the word CARE with 1, 2, 9, and
-- 6 respectively, we form a square number: 1296 = 36^2. What is
-- remarkable is that, by using the same digital substitutions, the
-- anagram, RACE, also forms a square number: 9216 = 96^2. We shall
-- call CARE (and RACE) a square anagram word pair and specify further
-- that leading zeroes are not permitted, neither may a different
-- letter have the same digital value as another letter.

-- Using words.txt (right click and 'Save Link/Target As...'), a 16K
-- text file containing nearly two-thousand common English words, find
-- all the square anagram word pairs (a palindromic word is NOT
-- considered to be an anagram of itself).

-- What is the largest square number formed by any member of such a
-- pair?

-- NOTE: All anagrams formed must be contained in the given text file.

module Euler098 where

import Data.List
import Data.Function (on)
import Data.Maybe

combinations :: [a] -> Int -> [[a]]
combinations [] _ = []
combinations xs 1 = map (\x -> [x]) xs
combinations (x:xs) n = combinations xs n ++ (map (x:) $ combinations xs (n-1))

enumerate :: Int -> [[Int]]
enumerate = concatMap permutations . combinations [0..9]

tidyStrings :: [String] -> [(String, String)]
tidyStrings ls = map (\[x,y]-> (x,y)) $ concatMap (flip combinations 2) candidate
  where candidate = filter ((>=2).length) . groupBy ((==) `on` sort) . sortBy (compare `on` sort) $ ls

isSquare :: Integer -> Bool
isSquare n = n == r * r
  where r = helper n
        helper m = fst . head . dropWhile (\(a,b) -> a + 1 /= b && a /= b) $ iterate (\(_,x) -> (x, (x + div m x) `div` 2)) (2, 1)

judgePair :: (String, String) -> [(Integer, Integer)]
judgePair (s1, s2)
  | length chars > 10 = []
  | otherwise = [(num1, num2) | (num1, num2) <- numberPairs, isSquare num1 && isSquare num2]
  where chars = map head $ group $ sort s1
        numberPairs = [(trans s1, trans s2) |
                       seq <- enumerate (length chars),
                       let mapping = zip chars seq,
                       let mapChar ch = fromJust $ lookup ch mapping,
                       mapChar (head s1) /= 0,
                       mapChar (head s2) /= 0,
                       let trans = foldl1 (\x y -> x* 10 + y) . map (fromIntegral . mapChar)]

getMaxSquare :: String -> Integer
getMaxSquare xs = maximum $ map fst pairs ++ map snd pairs
  where pairs = concatMap judgePair $ tidyStrings $ read $ "[" ++ xs ++ "]"

result098 = do
  content <- readFile "data/words.txt"
  print $ getMaxSquare content
