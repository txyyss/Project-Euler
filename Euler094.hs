-- It is easily proved that no equilateral triangle exists with
-- integral length sides and integral area. However, the almost
-- equilateral triangle 5-5-6 has an area of 12 square units.

-- We shall define an almost equilateral triangle to be a triangle for
-- which two sides are equal and the third differs by no more than one
-- unit.

-- Find the sum of the perimeters of all almost equilateral triangles
-- with integral side lengths and area and whose perimeters do not
-- exceed one billion (1,000,000,000).

module Euler094 where

-- http://www.mathblog.dk/project-euler-94-almost-equilateral-triangles/

solvePell :: Num t => t -> t -> t -> [(t, t)]
solvePell n x1 y1 = iterate helper (x1,y1)
  where helper (x,y) = (x1 * x + n * y1 * y, x1 * y + y1 * x)

-- b = a + 1
checkBAp1 :: Integral a => (a, a) -> a
checkBAp1 (x,y)
  | isLegal = 2 * x
  | otherwise = 0
  where a3 = 2 * x - 1
        area3 = y * (x - 2)
        isLegal = a3 > 0 && area3 > 0 && a3 `mod` 3 == 0 &&  area3 `mod` 3 == 0

-- b = a - 1
checkBAm1 :: Integral a => (a, a) -> a
checkBAm1 (x,y)
  | isLegal = 2 * x
  | otherwise = 0
  where a3 = 2 * x + 1
        area3 = y * (x + 2)
        isLegal = a3 > 0 && area3 > 0 && a3 `mod` 3 == 0 &&  area3 `mod` 3 == 0

result094 = smap checkBAm1 + smap checkBAp1
  where smap func = sum $ map func candidates
        candidates = takeWhile (\(x,_)-> 2 * x < (10^9)) $ solvePell 3 2 1
