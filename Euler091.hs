-- The points P (x1, y1) and Q (x2, y2) are plotted at integer
-- co-ordinates and are joined to the origin, O(0,0), to form ΔOPQ.

-- There are exactly fourteen triangles containing a right angle that
-- can be formed when each co-ordinate lies between 0 and 2 inclusive;
-- that is, 0 <= x1, y1, x2, y2 <= 2.

-- Given that 0 <= x1, y1, x2, y2 <= 50, how many right triangles can
-- be formed?

module Euler091 where

import Data.List

isRightTri :: ((Int,Int),(Int,Int)) -> Bool
isRightTri ((0,0),(_,_)) = False
isRightTri ((_,_),(0,0)) = False
isRightTri ((x1,y1),(x2,y2))
  | x1 == x2 && y1 == y2 = False
  | x1 * y2 == x2 * y1 = False
  | a + b == c = True
  | otherwise = False
  where d1 = x1^2 + y1^2
        d2 = x2^2 + y2^2
        d3 = (x1-x2)^2 + (y1-y2)^2
        [a,b,c] = sort [d1,d2,d3]

result091 = (`div` 2) $ length $ filter isRightTri [((x1,y1),(x2,y2)) | x1<-[0..50],x2<-[0..50],y1<-[0..50],y2<-[0..50]]
