-- Euler's Totient function, φ(n) [sometimes called the phi function],
-- is used to determine the number of positive numbers less than or
-- equal to n which are relatively prime to n. For example, as 1, 2,
-- 4, 5, 7, and 8, are all less than nine and relatively prime to
-- nine, φ(9)=6.

-- The number 1 is considered to be relatively prime to every positive
-- number, so φ(1)=1.

-- Interestingly, φ(87109)=79180, and it can be seen that 87109 is a
-- permutation of 79180.

-- Find the value of n, 1 < n < 10^7, for which φ(n) is a permutation
-- of n and the ratio n/φ(n) produces a minimum.

module Euler070 where

import Data.Array
import Data.List
import Data.Ord
import Data.Ratio

minus :: Ord a => [a] -> [a] -> [a]
minus lx@(x:xs) ly@(y:ys) = case compare x y of 
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

findFirstFactor :: Int -> (Int, Int)
findFirstFactor n = helper . dropWhile (\x->n `mod` x/=0) $ takeWhile (<=(ceiling . sqrt $ fromIntegral n)) primes
  where helper [] = (n,1)
        helper (x:_) = (x, countPow x)
        countPow m = length . takeWhile (\(a,b)->b==0) . scanl (\(a,b) x -> (div a x, mod a x)) (div n m, 0) $ repeat m

eulerTotients :: Int -> Array Int Int
eulerTotients n = result
  where result = listArray (2,n) $ map phi [2..n]
        phi m = helper $ findFirstFactor m
          where helper (p, k)
                  | fstPrd == m = p^(k-1) * (p-1)
                  | otherwise = (result ! fstPrd) * (result ! sndPrd)
                  where fstPrd = p^k
                        sndPrd = m `div` fstPrd

isPermutation :: (Int, Int) -> Bool
isPermutation (x, y) = sx == sy
  where sx = helper x
        sy = helper y
        helper = sort . show


result070 = minimumBy (comparing snd) . map (\(x,y) -> (x, fromIntegral x % fromIntegral y)) . filter isPermutation . assocs . eulerTotients $ 10^7
