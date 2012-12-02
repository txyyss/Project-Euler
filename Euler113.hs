-- Working from left-to-right if no digit is exceeded by the digit to
-- its left it is called an increasing number; for example, 134468.

-- Similarly if no digit is exceeded by the digit to its right it is
-- called a decreasing number; for example, 66420.

-- We shall call a positive integer that is neither increasing nor
-- decreasing a "bouncy" number; for example, 155349.

-- As n increases, the proportion of bouncy numbers below n increases
-- such that there are only 12951 numbers below one-million that are
-- not bouncy and only 277032 non-bouncy numbers below 10^10.

-- How many numbers below a googol (10^100) are not bouncy?

module Euler113 where

-- see the formula in
-- http://www.mathblog.dk/project-euler-113-googol-not-bouncy/

choose :: Integral a => a -> a -> a
choose m n = product [(n+1)..m] `div` product [2..(m-n)]

notBouncyCount :: Integral a => a -> a
notBouncyCount n = choose (n+10) 10 + choose (n+9) 9 - 2 - 10 * n

result113 = notBouncyCount 100
