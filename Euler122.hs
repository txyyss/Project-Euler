-- The most naive way of computing n¹⁵ requires fourteen
-- multiplications:

-- n × n × ... × n = n¹⁵

-- But using a "binary" method you can compute it in six
-- multiplications:

-- n × n = n²
-- n² × n² = n⁴
-- n⁴ × n⁴ = n⁸
-- n⁸ × n⁴ = n¹²
-- n¹² × n² = n¹⁴
-- n¹⁴ × n = n¹⁵

-- However it is yet possible to compute it in only five multiplications:

-- n × n = n²
-- n² × n = n³
-- n³ × n³ = n⁶
-- n⁶ × n⁶ = n¹²
-- n¹² × n³ = n¹⁵

-- We shall define m(k) to be the minimum number of multiplications to
-- compute nᵏ; for example m(15) = 5.

-- For 1 ≤ k ≤ 200, find ∑m(k).

module Euler122 where

-- http://oeis.org/A003313

-- From TAOCP Volume 2, 3rd, P.465, the "power tree" method is optimum
-- for k <> 77,154,233. The algorithm to generate power tree can be found
-- in P.481, Exercise 5.

import Data.Array
import Data.List

traceBack :: Array Int Int -> Int -> [Int]
traceBack arr i = helper arr [i]
  where helper a r@(x:_)
          | ax == 0 = r
          | otherwise = helper a (ax : r)
          where ax = a ! x

genPowerTree :: Int -> Array Int Int
genPowerTree bound = expandLayer initU initR 1
  where initU = listArray (1, bound) (take bound $ repeat (-1)) // [(1,0)]
        initR = listArray (1, bound) (take bound $ repeat 0)
        isFull = all (>=0) . elems
        genNewLeaves arr i = filter (\x -> x <= bound && arr ! x < 0) $ map (+i) (traceBack arr i)
        expandNode linkU linkR i = (linkU // modiU, linkR // modiR, fstR, lastR)
          where newNodes = genNewLeaves linkU i
                modiU = map (\x -> (x, i)) newNodes
                modiR = zip newNodes (tail newNodes)
                fstR = if null newNodes then 0 else head newNodes
                lastR = if null newNodes then 0 else last newNodes
        iterateExpand linkU linkR fs ls [] = (linkU, linkR // (zip (init newLs) (tail newFs)), head newFs)
          where newFs = filter (/= 0) $ reverse fs
                newLs = filter (/= 0) $ reverse ls
        iterateExpand linkU linkR fs ls (x:xs) = iterateExpand newU newR (f:fs) (l:ls) xs
          where (newU, newR, f, l) = expandNode linkU linkR x
        expandLayer linkU linkR first
          | isFull linkU = linkU
          | otherwise = expandLayer newU newR newFirst
          where (newU, newR, newFirst) = iterateExpand linkU linkR [] [] (reverse $ traceBack linkR first)

result122 = map ((+ (-1)) . length . traceBack wholeTree) [1..200]
  where wholeTree = genPowerTree 200
