-- Let d(n) be defined as the sum of proper divisors of n (numbers
-- less than n which divide evenly into n).

-- If d(a) = b and d(b) = a, where a b, then a and b are an amicable
-- pair and each of a and b are called amicable numbers.

-- For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20,
-- 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of
-- 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

-- Evaluate the sum of all the amicable numbers under 10000.

module Euler where

properDivisors :: Int -> [Int]
properDivisors n = tryDivide [1] [2..(floor $ sqrt $ fromIntegral n)]
  where tryDivide currRslt [] = currRslt
        tryDivide currRslt (x:xs)
          | n `mod` x == 0 = if (x == remain && x * remain == n) 
                             then tryDivide (x:currRslt) xs 
                             else tryDivide (x:remain:currRslt) xs
          | otherwise = tryDivide currRslt xs
          where remain = n `div` x

d :: Int -> Int
d = sum . properDivisors

amicable :: Int -> Bool
amicable n = n /= dn && d dn == n
  where dn = d n

result021 = sum $ filter amicable [2..10000]
