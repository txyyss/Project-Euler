-- Special isosceles triangles

module Euler138 where

-- Detailed analysis can be found in
-- http://www.mathblog.dk/project-euler-138-special-isosceles-triangles/

result138 = sum $ map (abs . snd) $ tail $ take 13 $ iterate helper (0,1)
  where helper (x,y) = (- 9 * x - 4 * y - 4, - 20 * x - 9 * y - 8)
