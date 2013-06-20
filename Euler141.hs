-- Investigating progressive numbers, n, which are also square.

-- A positive integer, n, is divided by d and the quotient and
-- remainder are q and r respectively. In addition d, q, and r are
-- consecutive positive integer terms in a geometric sequence, but not
-- necessarily in that order.

-- For example, 58 divided by 6 has quotient 9 and remainder 4. It can
-- also be seen that 4, 6, 9 are consecutive terms in a geometric
-- sequence (common ratio 3/2).

-- We will call such numbers, n, progressive.

-- Some progressive numbers, such as 9 and 10404 = 102^2, happen to
-- also be perfect squares.

-- The sum of all progressive perfect squares below one hundred
-- thousand is 124657.

-- Find the sum of all progressive perfect squares below one trillion
-- (10^12).

module Euler141 where

-- datailed analysis:
-- http://www.mathblog.dk/project-euler-141investigating-progressive-numbers-n-which-are-also-square/

intSqrt n = fst . head . dropWhile (\(a,b) -> a + 1 /= b && a /= b) $ iterate (\(_,x) -> (x, (x + div n x) `div` 2)) (2, 1)

isPerfectSqr n = r * r == n
  where r = intSqrt n

getSum :: Integer -> Integer
getSum limit = sum $ filter isPerfectSqr $ concat [takeWhile (<limit) $ map (\c-> p * c * c + q * c) [1..] |
                             a <- [2..10000], b <- [1..(a-1)], gcd a b == 1,
                             let p = a * a * a * b, let q = b * b, p + q < limit]

result141 = getSum (10^12)
