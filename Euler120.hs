-- Let r be the remainder when (a-1)^n + (a+1)^n is divided by a^2.

-- For example, if a = 7 and n = 3, then r = 42: 6^3 + 8^3 = 728 = 42
-- mod 49. And as n varies, so too will r, but for a = 7 it turns out
-- that rmax = 42.

-- For 3 <= a <= 1000, find Sum rmax.

module Euler120 where

-- see http://www.mathblog.dk/project-euler-120-maximum-remainder/

result120 = sum [2 * a * ((a-1) `div` 2) | a <- [3..1000]]
