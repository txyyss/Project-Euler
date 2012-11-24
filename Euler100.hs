-- If a box contains twenty-one coloured discs, composed of fifteen
-- blue discs and six red discs, and two discs were taken at random,
-- it can be seen that the probability of taking two blue discs, P(BB)
-- = (15/21)(14/20) = 1/2.

-- The next such arrangement, for which there is exactly 50% chance of
-- taking two blue discs at random, is a box containing eighty-five
-- blue discs and thirty-five red discs.

-- By finding the first arrangement to contain over 10^12 =
-- 1,000,000,000,000 discs in total, determine the number of blue
-- discs that the box would contain.

module Euler100 where

-- See equations in
-- http://www.mathblog.dk/project-euler-100-blue-discs-two-blue/

-- next_b = 3 b + 2 n - 2
-- next_n = 4 b + 3 n - 3

result100 = fst . head . dropWhile ((<(10^12)) . snd) $ iterate (\(b,n) -> (3 * b + 2 * n - 2, 4 * b + 3 * n - 3)) (15, 21)
