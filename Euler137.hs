-- Fibonacci golden nuggets

module Euler137 where

-- Analysis:
--   A_F(x) = x F_1 + x^2 F_2 + x^3 F_3 + x^4 F_4 + ...
-- x A_F(x) =         x^2 F_1 + x^3 F_2 + x^4 F_3 + ...
-- (1-x) A_F(x) = x + x^2 A_F(x)
-- A_F(x) = x / (1 - x - x^2)
-- Solve A_F(x) = n, we get x = (sqrt(1 + 2n + 5n^2) - n - 1) / (2n)
-- So to find golden nuggets, we need 1 + 2n + 5n^2 to a perfect square.
-- try to enumerate the solution, we can observe that n = F_(2k) * F_(2k+1)

-- It can be proved. I cite one proof from a smart guy:
-- 1 + 2n + 5n^2 = (n+1)^2 + (2n)^2,  This is a Pythagorean triple.
-- --------------------
-- CASE: n is even
-- --------------------

-- When n is even, n+1 and 2n are coprime, so this is a primitive triple.
-- Then the triple must have the Euclid parametrization

-- n+1 = x^2 - y^2
-- 2n  = 2xy
-- -> x^2 - xy - y^2 = 1, n = xy

-- The solutions (x,y) to this quadratic Diophantine equation are among the convergents x/y of the roots of

-- z^2 - x - z = 0

-- There is one positive root phi = (1+sqrt(5))/2 which will lead to positive x and y (as required),
-- while the negative root will give negative solutions we don't want.
-- This phi is the golden ratio, whose convergents are ratios of successive Fibonacci numbers.
-- From this, it follows that all the solutions (x,y) are of the form

-- x = Fib(i+1)
-- y = Fib(i)

-- for all even integers i.  The odd integers i don't lead to solutions because of the following identity:

-- Fib(i+1)^2 - Fib(i)*Fib(i+1) - Fib(i)^2 = 1 if i is even or -1 if i is odd

-- Consequently, all the even n solutions are of the form

-- n = Fib(i) * Fib(i+1)

-- where i is an even integer. Of course, not every even i yields an even n.
-- For n to be even, at least one of Fib(i) and Fib(i+1) must be even.
-- Since the parity of the Fibonacci sequence goes like

-- i                   | 123456789... 
-- parity of Fib(i)    | OOEOOEOOE...
-- n is even           |  XX XX X   i = 0 or 2 (mod 3)
-- i is even           |  X X X X   i = 0 (mod 2)
-- both are even       |  X   X X   i = 0 or 2 (mod 6)

-- => So, n = Fib(i)*Fib(i+1) is an even solution for all i = 0 or 2 (mod 6)
-- (It should be noted that i = 0 -> n = 0, which is the trivial solution)

-- --------------------
-- CASE: n is odd
-- --------------------

-- When n is odd, n = 2k-1,

-- gcd(n+1,2n) = gcd(2k,4k-2) = 2 gcd(k,2k-1) = 2

-- so (n+1)/2 = k and 2n/2 = 2k-1  are coprime and form a primitive triple.
-- At least one of the bases of a primitive triple must be even, and that one must be k

-- 2k-1 = x^2 - y^2
-- k  = 2xy
-- -> y^2 + 4yx - x^2 = 1

-- The solutions (y,z) to this quadratic Diophantine equation are among the convergents y/x of the roots of

-- z^2 + 4z - 1 = 0

-- There is one positive root sqrt(5)-2, and again we don't care about the negative root.
-- This is related to the golden ratio phi by

-- sqrt(5)-2 = 2 phi - 3

-- The convergents of this number are (with the help Mathematica and OEIS)
-- are the reduced ratios of successive even Fibanocci numbers.
-- From this, it follows that all the solutions (x,y) are of the form

-- y = Fib(3j)/2
-- x = Fib(3(j+1))/2

-- for all odd integers j. Consequently, all the odd n solutions are of the form

-- k = 2xy = Fib(3j) * Fib(3j+3) / 2
-- n = 2k-1 = Fib(3j) * Fib(3j+3) - 1
--     = (Fib(3j+2) - Fib(3j+1)) * (Fib(3j+2) + Fib(3j+1)) - 1
--     = Fib(3j+2)^2 - Fib(3j+1)^2 - 1

-- Using the above identity, since odd j means that 3j+1 is even, all the odd n solutions
-- are of the form

-- n = Fib(3j+1) * Fib(3j+2)

-- where j is an odd integer. It happens every odd j yields an odd n.
-- Fib(3j+1) and Fib(3j+2) are odd for all j.
-- Defining i = 3j+1, j = 1 (mod 2) is equivalent to i = 4 (mod 6)

-- => So, n = Fib(i)*Fib(i+1) is an odd solution for all i = 4 (mod 6)
-- (It should be noted that i = 0 -> n = 0, which is the trivial solution)

-- ------------------------------
-- PUTTING THE TWO CASES TOGETHER
-- ------------------------------

-- We've found that n = Fib(i) * Fib(i+1) is a solution for all i = 0, 2, 4 (mod 6)
-- where n is even in the case i = 0, 2 (mod 6) and odd in the case i = 4 (mod 6)
-- But i = 0, 2, 4 (mod 6) is equivalent to i = 0 (mod 2).
-- So, all the solutions are given by Fib(i) * Fib(i+1) for even i.
-- Since i = 0 is the trivial solution n = 0, we can number
-- the non-trivial solutions 1,2,3,..., and the k-th solution is

-- n(k) = Fib(2k) * Fib(2k+1)

import Data.Array

fibonacciArr :: Int -> Array Int Integer
fibonacciArr n = arr
  where arr = listArray (1, n) $ map helper [1..n]
        helper 1 = 1
        helper 2 = 1
        helper k = (arr ! (k-1)) + (arr ! (k-2))

result137 = (arr ! 30) * (arr ! 31)
  where arr = fibonacciArr 31
