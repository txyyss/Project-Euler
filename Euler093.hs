-- By using each of the digits from the set, {1, 2, 3, 4}, exactly
-- once, and making use of the four arithmetic operations (+, , *, /)
-- and brackets/parentheses, it is possible to form different positive
-- integer targets.

-- For example,

-- 8 = (4 * (1 + 3)) / 2
-- 14 = 4 * (3 + 1 / 2)
-- 19 = 4 * (2 + 3)  1
-- 36 = 3 * 4 * (2 + 1)

-- Note that concatenations of the digits, like 12 + 34, are not allowed.

-- Using the set, {1, 2, 3, 4}, it is possible to obtain thirty-one
-- different target numbers of which 36 is the maximum, and each of
-- the numbers 1 to 28 can be obtained before encountering the first
-- non-expressible number.

-- Find the set of four distinct digits, a < b < c < d, for which the
-- longest set of consecutive positive integers, 1 to n, can be
-- obtained, giving your answer as a string: abcd.

module Euler093 where

import Data.Ratio
import Data.List
import qualified Data.Set as Set
import Data.Maybe
import Data.Ord

combinations :: Int -> [a] -> [[a]]
combinations _ [] = []
combinations 1 xs = map (:[]) xs
combinations n (x:xs) = combinations n xs ++ map (x:) (combinations (n-1) xs)

genAllPossible :: [Int] -> Set.Set (Ratio Int)
genAllPossible ls = Set.fromList $ catMaybes $ concat [gen5 xs op1 op2 op3 | xs<-perm,op1<-ops,op2<-ops,op3<-ops]
  where perm = permutations $ map (%1) ls
        divM (Just a) (Just b)
          | numerator b == 0 = Nothing
          | otherwise = Just (a / b)
        divM Nothing _ = Nothing
        divM _ Nothing = Nothing
        minusM (Just a) (Just b) = Just (a-b)
        minusM Nothing _ = Nothing
        minusM _ Nothing = Nothing
        plusM (Just a) (Just b) = Just (a+b)
        plusM Nothing _ = Nothing
        plusM _ Nothing = Nothing
        multM (Just a) (Just b) = Just (a*b)
        multM Nothing _ = Nothing
        multM _ Nothing = Nothing
        ops = [multM, plusM, minusM, divM]
        gen5 li o1 o2 o3 = [((a `o1` b) `o2` c) `o3` d,
                            a `o1` (b `o2` (c `o3` d)),
                            (a `o1` (b `o2` c)) `o3` d,
                            a `o1` ((b `o2` c) `o3` d),
                            (a `o1` b) `o2` (c `o3` d)]
          where [a,b,c,d] = map Just li

maxConsecutive :: Set.Set (Ratio Int) -> Int
maxConsecutive aSet = helper $ takeWhile setContain [1..]
  where helper [] = 0
        helper xs = last xs
        setContain n = Set.member (n%1) aSet

result093 = sort $ maximumBy (comparing (maxConsecutive . genAllPossible)) $ combinations 4 [1..9]
