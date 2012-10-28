-- Consider quadratic Diophantine equations of the form:

-- x^2 – D y^2 = 1

-- For example, when D=13, the minimal solution in x is 649^2 – 13 *
-- 180^2 = 1.

-- It can be assumed that there are no solutions in positive integers
-- when D is square.

-- By finding minimal solutions in x for D = {2, 3, 5, 6, 7}, we
-- obtain the following:

-- 3^2 – 2 * 2^2 = 1
-- 2^2 – 3 * 1^2 = 1
-- 9^2 – 5 * 4^2 = 1
-- 5^2 – 6 * 2^2 = 1
-- 8^2 – 7 * 3^2 = 1

-- Hence, by considering minimal solutions in x for D <= 7, the
-- largest x is obtained when D=5.

-- Find the value of D <= 1000 in minimal solutions of x for which the
-- largest value of x is obtained.

module Euler066 where

import Data.List
import Data.Ord
import Data.Ratio

nextTuple :: (Int, Int, Int, Int, Int) -> (Int, Int, Int, Int, Int)
nextTuple (a,b,c,d,e) = (newA, tB `div` comD, c, tD `div` comD, tE `div` comD)
  where newA = floor $ (fromIntegral tB * (sqrt $ fromIntegral c) - fromIntegral (e * d)) / (fromIntegral tE)
        tB = e * b
        tD = (-e) * d - newA * tE
        tE = b * b * c - d * d
        comD = gcd tD $ gcd tB tE

contFrac :: Int -> [Int]
contFrac n = intRoot : (concatMap (\x->segment) $ repeat 1)
  where intRoot = floor . sqrt $ fromIntegral n
        segment = helper [(intRoot, 1, n, (-intRoot), 1)]
        helper ls@(x:_)
          | elem newX ls = reverse . init $ map (\(x,_,_,_,_)->x) ls
          | otherwise = helper (newX:ls)
          where newX = nextTuple x
                addOrReturn False = helper (newX:ls)

genApproxList :: Int -> [(Integer, Integer)]
genApproxList n = map (\x -> (numerator x, denominator x)) $ map approxFract [1..]
  where conts = contFrac n
        approxFract m = foldr1 (\x y-> x + 1/y) . map (%1) $ map toInteger $ take m conts

solvePell :: Int -> (Integer, Integer)
solvePell n = helper solutions
  where solutions = genApproxList n
        helper (s@(x,y):xs)
          | x * x - toInteger n * y * y == 1 = s
          | otherwise = helper xs

result066 = fst . maximumBy (comparing snd) . zip potential . map fst $ map solvePell potential
  where potential = [1..1000] \\ (map (^2) [1..32])
