-- Pythagorean tiles

module Euler139 where

primitivePythagoreanTri :: Int -> [Int]
primitivePythagoreanTri limit = [sum |
                                 m <- [1..maxM], n <- [1..(m-1)], gcd m n == 1, even m || even n,
                                 let sum = 2 * m * (m + n),sum <= limit,
                                 let a = m * m - n * n, let b = 2 * m * n, let c = m * m + n * n,
                                 c `mod` abs (b-a)==0]
  where maxM = ceiling $ sqrt (fromIntegral limit / 2)

result139 = sum $ map (100000000 `div`) $ primitivePythagoreanTri 100000000
