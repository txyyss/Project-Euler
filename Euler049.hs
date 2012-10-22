-- The arithmetic sequence, 1487, 4817, 8147, in which each of the
-- terms increases by 3330, is unusual in two ways: (i) each of the
-- three terms are prime, and, (ii) each of the 4-digit numbers are
-- permutations of one another.

-- There are no arithmetic sequences made up of three 1-, 2-, or
-- 3-digit primes, exhibiting this property, but there is one other
-- 4-digit increasing sequence.

-- What 12-digit number do you form by concatenating the three terms
-- in this sequence?

module Euler049 where

import Data.List (sort, group, permutations)
import Data.Char (digitToInt)

minus :: Ord a => [a] -> [a] -> [a]
minus lx@(x:xs) ly@(y:ys) = case (compare x y) of 
  LT -> x : minus xs ly
  EQ ->     minus xs ys 
  GT ->     minus lx ys
minus xs _ = xs

primes = 2 : primes'
  where
    primes' = sieve [3,5..] 9 primes'
    sieve (x:xs) q ps@ ~(p:t)
      | x < q     = x : sieve xs q ps
      | otherwise =     sieve (xs `minus` [q, q+2*p..]) (head t^2) t

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = helper primes
  where helper (x:xs)
          | x * x > n = True
          | n `mod` x == 0 = False
          | otherwise = helper xs

permutateNum = map (foldl1 (\x y -> x * 10 + y)) . permutations . map digitToInt . show

samePermutation n m = (norm n) == (norm m)
  where norm x = sort $ show x

judge n m = n < m && samePermutation (2 * m - n) n

findFor n = n : (filter (judge n) . map head . group . sort . tail $ permutateNum n)

reNorm xs = map (\x-> [head xs, x]) $ tail xs

nextIsPrime (x:y:xs) = isPrime (2 * y - x)

finalForm (x:y:xs) = (show x) ++ (show y) ++ (show (2 * y - x))

result049 = map finalForm . filter nextIsPrime . concatMap reNorm . 
            filter (and . (map isPrime)) . filter (\x -> length x >= 2) $ map findFor [1000..9999]
