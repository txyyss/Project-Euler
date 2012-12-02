-- Considering 4-digit primes containing repeated digits it is clear
-- that they cannot all be the same: 1111 is divisible by 11, 2222 is
-- divisible by 22, and so on. But there are nine 4-digit primes
-- containing three ones:

-- 1117, 1151, 1171, 1181, 1511, 1811, 2111, 4111, 8111

-- We shall say that M(n, d) represents the maximum number of repeated
-- digits for an n-digit prime where d is the repeated digit, N(n, d)
-- represents the number of such primes, and S(n, d) represents the
-- sum of these primes.

-- So M(4, 1) = 3 is the maximum number of repeated digits for a
-- 4-digit prime where one is the repeated digit, there are N(4, 1) =
-- 9 such primes, and the sum of these primes is S(4, 1) = 22275. It
-- turns out that for d = 0, it is only possible to have M(4, 0) = 2
-- repeated digits, but there are N(4, 0) = 13 such cases.

-- In the same way we obtain the following results for 4-digit primes.

-- Digit, d	M(4, d)	N(4, d)	S(4, d)
-- 0	2	13	67061
-- 1	3	9	22275
-- 2	3	1	2221
-- 3	3	12	46214
-- 4	3	2	8888
-- 5	3	1	5557
-- 6	3	1	6661
-- 7	3	9	57863
-- 8	3	1	8887
-- 9	3	7	48073

-- For d = 0 to 9, the sum of all S(4, d) is 273700.

-- Find the sum of all S(10, d).

module Euler111 where

-- I use the solution of
-- https://github.com/bjin/puzzles/blob/master/projecteuler.net/111.hs
-- be careful of replicateM and sequence function on List

import Control.Monad
import Data.List
import Data.Function
import Data.Ord

minus :: Ord a => [a] -> [a] -> [a]
minus lx@(x:xs) ly@(y:ys) = case (compare x y) of 
  LT -> x : minus xs ly
  EQ ->     minus xs ys 
  GT ->     minus lx ys
minus xs _ = xs

primes :: [Integer]
primes = 2 : primes'
  where
    primes' = sieve [3,5..] 9 primes'
    sieve (x:xs) q ps@ ~(p:t)
      | x < q     = x : sieve xs q ps
      | otherwise =     sieve (xs `minus` [q, q+2*p..]) (head t^2) t

isPrime :: Integer -> Bool
isPrime n = helper primes
  where helper (x:xs)
          | x * x > n = True
          | n `mod` x == 0 = False
          | otherwise = helper xs

generateNums :: Int -> Integer -> [(Int, [Integer])]
generateNums n d = [(size, map (foldl1 (\x y -> x * 10 + y)) $ sequence candidate) |
                    possible <- replicateM n [delete d [0..9], [d]],
                    let size = length $ filter ((==1).length) possible,
                    let candidate = (delete 0 $ head possible) : tail possible]

maxRepeatedPrimes :: Int -> Integer -> [Integer]
maxRepeatedPrimes n = head . filter (not.null) . map (filter isPrime . (concatMap snd)) .
                      groupBy ((==) `on` fst) . reverse . sortBy (comparing fst) . generateNums n

result111 = sum $ map (sum . maxRepeatedPrimes 10) [0..9]
