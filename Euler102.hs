-- Three distinct points are plotted at random on a Cartesian plane,
-- for which -1000 <= x, y <= 1000, such that a triangle is formed.

-- Consider the following two triangles:

-- A(-340,495), B(-153,-910), C(835,-947)

-- X(-175,41), Y(-421,-714), Z(574,-645)

-- It can be verified that triangle ABC contains the origin, whereas
-- triangle XYZ does not.

-- Using triangles.txt (right click and 'Save Link/Target As...'), a
-- 27K text file containing the co-ordinates of one thousand "random"
-- triangles, find the number of triangles for which the interior
-- contains the origin.

-- NOTE: The first two examples in the file represent the triangles in
-- the example given above.

module Euler102 where

vMinus (a,b) (c,d) = (a-b,c-d)
cross (a,b) (c,d) = a * d - b * c
lineEquation (x1,y1) (x2,y2) (x,y) = (y-y1) * (x2-x1) - (y2-y1)*(x-x1)

containPoint :: (Num a, Ord a) => (a, a) -> [a] -> Bool
containPoint (x,y) [a,b,c,d,e,f]
  | clockwise = helper (a,b) (e,f) (c,d) (x,y)
  | otherwise = helper (a,b) (c,d) (e,f) (x,y)
  where clockwise = cross (vMinus (c,d) (a,b)) (vMinus (e,f) (a,b)) < 0
        helper p1 p2 p3 p = all (>0) side || all (<0) side
          where side = [lineEquation p1 p2 p, lineEquation p2 p3 p, lineEquation p3 p1 p]

toPoints :: String -> [Int]
toPoints s = read $ "["++ s ++"]"

result102 = fmap (length . filter (==True) . map (containPoint (0,0) . toPoints) . lines)$ readFile "data/triangles.txt"
