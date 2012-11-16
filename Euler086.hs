-- A spider, S, sits in one corner of a cuboid room, measuring 6 by 5
-- by 3, and a fly, F, sits in the opposite corner. By travelling on
-- the surfaces of the room the shortest "straight line" distance from
-- S to F is 10 and the path is shown on the diagram.

-- However, there are up to three "shortest" path candidates for any
-- given cuboid and the shortest route doesn't always have integer
-- length.

-- By considering all cuboid rooms with integer dimensions, up to a
-- maximum size of M by M by M, there are exactly 2060 cuboids for
-- which the shortest route has integer length when M=100, and this is
-- the least value of M for which the number of solutions first
-- exceeds two thousand; the number of solutions is 1975 when M=99.

-- Find the least value of M such that the number of solutions first
-- exceeds one million.

module Euler086 where

import Data.List
import Data.Maybe

isSquare :: Integer -> Bool
isSquare n = n == r * r
  where r = helper n
        helper m = fst . head . dropWhile (\(a,b) -> a + 1 /= b && a /= b) $ iterate (\(_,x) -> (x, (x + div m x) `div` 2)) (2, 1)

perfectUnder :: Integer -> Integer
perfectUnder c = sum $ map (\n-> min (n-1) c + 1 + n `div` 2 - n) [n | n<-[2..c * 2], isSquare (n * n + c * c)]

numOfIntPaths = scanl (+) 0 $ map perfectUnder [1..]

result086 = fromJust $ findIndex (> 1000000) numOfIntPaths
