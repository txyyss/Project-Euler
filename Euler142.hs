-- Perfect Square Collection

-- Find the smallest x + y + z with integers x y z 0 such that x + y,
-- x y, x + z, x z, y + z, y z are all perfect squares.

module Euler142 where

-- see analysis in
-- http://www.mathblog.dk/project-euler-142-perfect-square-collection/

intSqrt n = fst . head . dropWhile (\(a,b) -> a + 1 /= b && a /= b) $ iterate (\(_,x) -> (x, (x + div n x) `div` 2)) (2, 1)

isPerfectSqr n = r * r == n
  where r = intSqrt n

result142 = head [x + y + z |
                  i <- [4..], let a = i * i,
                  j <- [3..(i - 1)], let c = j * j,
                  let f = a - c, f > 0 && isPerfectSqr f,
                  let kStart = if even j then 2 else 1,
                  k <- [kStart, (kStart + 2)..(j-1)],
                  let d = k * k, let e = a - d, let b = c - e,
                  e > 0 && b > 0 && isPerfectSqr e && isPerfectSqr b,
                  let x = (a + b) `div` 2, let y = (e + f) `div` 2, let z = (c - d) `div` 2]
