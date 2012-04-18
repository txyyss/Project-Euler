-- If p is the perimeter of a right angle triangle with integral
-- length sides, {a,b,c}, there are exactly three solutions for p =
-- 120.

-- {20,48,52}, {24,45,51}, {30,40,50}

-- For which value of p <= 1000, is the number of solutions maximised?

module Euler where

-- See Euler 009

import Data.List (sort, group, maximumBy)
import Data.Ord (comparing)

mnk = [(m, n, k) | n <- [1..32], m <- [(n+1)..32], k <- [1..83], m * (m + n) * k <= 500]

triplet (m, n, k) = ((min a b), (max a b), c) 
  where a = k * (m^2 - n^2)
        b = k * 2 * m * n
        c = k * (m^2 + n^2)

allPerimeter = sort . map (\(x,y,z) -> x + y + z) . map head . group . sort $ map triplet mnk

result039 = maximumBy (comparing snd) $ map (\x -> (head x, length x)) . group $ allPerimeter
