-- What is the maximum 16-digit string for a "magic" 5-gon ring?

module Euler068 where

import Data.List
import Data.Char

partitionPentagon :: [Int] -> [[Int]]
partitionPentagon ls = map (map (ls!!)) subscripts
  where subscripts = [[0,5,6],[1,6,7],[2,7,8],[3,8,9],[4,9,5]]

isMagicPentagon :: [Int] -> Bool
isMagicPentagon ls = all (head sumList ==) $ tail sumList
  where sumList = map sum $ partitionPentagon ls

uniqueRep :: [Int] -> [Int]
uniqueRep ls = helper minIndex fstHalf ++ helper minIndex sndHalf
  where fstHalf = take 5 ls
        sndHalf = drop 5 ls
        minNum = minimum fstHalf
        minIndex = elemIndex minNum fstHalf
        helper Nothing = const []
        helper (Just minIdx) = take 5 . drop minIdx . cycle

convertToInteger :: [Int] -> Integer
convertToInteger = foldl1 (\x y -> x * 10 + y) . map (toInteger . digitToInt) . concatMap show . concat . partitionPentagon

result068 = maximum . map (convertToInteger . uniqueRep) .filter isMagicPentagon . map (10:) $ permutations [1..9]
