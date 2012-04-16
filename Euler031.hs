-- In England the currency is made up of pound, £, and pence, p, and there are eight coins in general circulation:

-- 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
-- It is possible to make £2 in the following way:

-- 1*£1 + 1*50p + 2*20p + 1*5p + 1*2p + 3 * 1p
-- How many different ways can £2 be made using any number of coins?

module Euler where

changeCount :: Int -> [Int] -> Int
changeCount 0 _ = 1
changeCount _ [] = 0
changeCount num l@(x:xs)
  | num < 0 = 0
  | otherwise = changeCount num xs + changeCount (num - x) l

result031 = changeCount 200 [200,100,50,20,10,5,2,1]
