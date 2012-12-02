-- Working from left-to-right if no digit is exceeded by the digit to
-- its left it is called an increasing number; for example, 134468.

-- Similarly if no digit is exceeded by the digit to its right it is
-- called a decreasing number; for example, 66420.

-- We shall call a positive integer that is neither increasing nor
-- decreasing a "bouncy" number; for example, 155349.

-- Clearly there cannot be any bouncy numbers below one-hundred, but
-- just over half of the numbers below one-thousand (525) are
-- bouncy. In fact, the least number for which the proportion of
-- bouncy numbers first reaches 50% is 538.

-- Surprisingly, bouncy numbers become more and more common and by the
-- time we reach 21780 the proportion of bouncy numbers is equal to
-- 90%.

-- Find the least number for which the proportion of bouncy numbers is
-- exactly 99%.

module Euler112 where

isIncreasing :: Ord b => [b] -> Bool
isIncreasing ls = and $ zipWith (<=) ls $ tail ls

isBouncy :: Int -> Bool
isBouncy n = not (isIncreasing s || isIncreasing (reverse s))
  where s = show n

leastBouncyCount :: Int -> Int
leastBouncyCount percent = helper 0 0
  where helper total bouncyCount
          | nextCount * 100 == percent * nextTotal = nextTotal
          | otherwise = helper nextTotal nextCount
          where nextTotal = total + 1
                nextCount = bouncyCount + if isBouncy nextTotal then 1 else 0

result112 = leastBouncyCount 99
