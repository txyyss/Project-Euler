-- By counting carefully it can be seen that a rectangular grid
-- measuring 3 by 2 contains eighteen rectangles:

-- Although there exists no rectangular grid that contains exactly two
-- million rectangles, find the area of the grid with the nearest
-- solution.

module Euler085 where

import Data.List
import Data.Ord

sumTo :: Int -> Int
sumTo n = n * (n + 1) `div` 2

nearestM :: Int -> Int -> Int
nearestM target n = result
  where sumToN = sumTo n
        t = fromIntegral target / fromIntegral sumToN
        m = floor ((sqrt (8 * t + 1) - 1) / 2)
        candidates = map (\x -> (x, sumToN * sumTo x)) [m..]
        neighbours = (head $ dropWhile ((<=target) . snd) candidates) : (takeWhile ((<=target) . snd) candidates)
        result = fst $ minimumBy (comparing (\x -> abs(target - snd x))) neighbours

result085 = (\(n,m) -> n * m) $ minimumBy (comparing (\(n,m) -> abs(2000000 - (sumTo n * sumTo m)))) $ map (\n -> (n, nearestM 2000000 n)) [1..2000]
