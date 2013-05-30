-- A bag contains one red disc and one blue disc. In a game of chance
-- a player takes a disc at random and its colour is noted. After each
-- turn the disc is returned to the bag, an extra red disc is added,
-- and another disc is taken at random.

-- The player pays £1 to play and wins if they have taken more blue
-- discs than red discs at the end of the game.

-- If the game is played for four turns, the probability of a player
-- winning is exactly 11/120, and so the maximum prize fund the banker
-- should allocate for winning in this game would be £10 before they
-- would expect to incur a loss. Note that any payout will be a whole
-- number of pounds and also includes the original £1 paid to play the
-- game, so in the example given the player actually wins £9.

-- Find the maximum prize fund that should be allocated to a single
-- game in which fifteen turns are played.

module Euler121 where

import Data.Ratio

maxRed n
  | even n = n `div` 2 - 1
  | otherwise = n `div` 2

combinations :: Int -> [a] -> [[a]]
combinations _ [] = []
combinations 1 xs = map (:[]) xs
combinations n (x:xs) = combinations n xs ++ map (x:) (combinations (n-1) xs)

prob :: Int -> Rational
prob n = num % denom
  where denom = product [2..fromIntegral (n+1)]
        num = 1 + sum (map sumComb [1..(maxRed n)])
        sumComb i = sum $ map product $ combinations i [1..(fromIntegral n)]

result121 = floor $ dp / np
  where prob15 = prob 15
        dp = fromIntegral $ denominator prob15
        np = fromIntegral $ numerator prob15
