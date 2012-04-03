-- 2520 is the smallest number that can be divided by each of the
-- numbers from 1 to 10 without any remainder.

-- What is the smallest positive number that is evenly divisible by
-- all of the numbers from 1 to 20?

module Euler where

result005 = foldr1 lcm [1..20]
