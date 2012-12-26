-- A palindromic number reads the same both ways. The largest
-- palindrome made from the product of two 2-digit numbers is 9009 =
-- 91 * 99.

-- Find the largest palindrome made from the product of two 3-digit
-- numbers.

module Euler004 where

-- natural version
isPalindrome :: Int -> Bool
isPalindrome x = helper $ show x
  where helper :: Eq a => [a] -> Bool
        helper [] = True
        helper [x] = True
        helper x
          | head x /= last x = False
          | otherwise = helper $ init $ tail x

result004 = maximum $ filter isPalindrome [x*y | x <- [999,998..100], y <-[x,(x-1)..100]]

-- improved version
findMax :: Int -> Int -> [Int] -> Int
findMax currMax a [] = currMax
findMax currMax a (b:bs)
  | pro < currMax = currMax
  | isPalindrome pro = if pro > currMax then pro else currMax
  | otherwise = findMax currMax a bs
  where pro = a * b

findMaxPalindrome :: Int -> [Int] -> Int
findMaxPalindrome currM [] = currM
findMaxPalindrome currM (x:xs) = max nextM (findMaxPalindrome nextM xs)
  where nextM = findMax currM x (getBs x)
        getBs y
          | y `mod` 11 == 0 = [y,(y-1)..100]
          | otherwise = [b,(b-11)..100]
          where b = y `div` 11 * 11

result004' = findMaxPalindrome 10000 [999,998..100]
