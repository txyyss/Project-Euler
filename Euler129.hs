-- A number consisting entirely of ones is called a repunit. We shall
-- define R(k) to be a repunit of length k; for example, R(6) =
-- 111111.

-- Given that n is a positive integer and GCD(n, 10) = 1, it can be
-- shown that there always exists a value, k, for which R(k) is
-- divisible by n, and let A(n) be the least such value of k; for
-- example, A(7) = 6 and A(41) = 5.

-- The least value of n for which A(n) first exceeds ten is 17.

-- Find the least value of n for which A(n) first exceeds one-million.

module Euler129 where

-- A (n) <= n

-- because R(1)..R(n) will have at most n different reminders and one
-- of them has to be 0. If there is no 0 reminder then 2 of those have
-- the same reminder R(x) = R(y) according to the pigeonhole
-- principle, their difference can be describes as R(q)*(10^p) where q
-- is less than (x or y) and this number is a multiple of n, since
-- GCD(n, 10) = 0 => R(q) is a multiple of n.

calA n = (+1) . length . takeWhile (/=0) $ iterate (\x -> (x * 10 + 1) `mod` n) 1

result129 = fst . head . dropWhile ((<=1000000) . snd) . map (\n -> (n, calA n)) $ filter ((==1) . gcd 10) [1000000..]
