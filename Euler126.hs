-- Cuboid layers

-- The minimum number of cubes to cover every visible face on a cuboid
-- measuring 3 x 2 x 1 is twenty-two.

-- If we then add a second layer to this solid it would require
-- forty-six cubes to cover every visible face, the third layer would
-- require seventy-eight cubes, and the fourth layer would require
-- one-hundred and eighteen cubes to cover every visible face.

-- However, the first layer on a cuboid measuring 5 x 1 x 1 also
-- requires twenty-two cubes; similarly the first layer on cuboids
-- measuring 5 x 3 x 1, 7 x 2 x 1, and 11 x 1 x 1 all contain
-- forty-six cubes.

-- We shall define C(n) to represent the number of cuboids that
-- contain n cubes in one of its layers. So C(22) = 2, C(46) = 4,
-- C(78) = 5, and C(118) = 8.

-- It turns out that 154 is the least value of n for which C(n) = 10.

-- Find the least value of n for which C(n) = 1000.

module Euler126 where

-- I just copy and understand the answer of bjin:
-- https://github.com/bjin/puzzles/blob/master/projecteuler.net/126.hs
-- because it's very elegant!

unionInf :: (Ord a, Num b) => [(a, b)] -> [(a, b)] -> [(a, b)]
unionInf (x:xs) (y:ys) = case compare (fst x) (fst y) of
    LT -> x : unionInf xs (y:ys)
    GT -> y : unionInf (x:xs) ys
    EQ -> (fst x, snd x + snd y) : unionInf xs ys

joinInf :: (Ord a, Num b) => [[(a, b)]] -> [(a, b)]
joinInf ((x:xs):ys) = x : unionInf xs (joinInf (pairsInf ys))

pairsInf :: (Ord a, Num b) => [[(a, b)]] -> [[(a, b)]]
pairsInf ((x:xs):ys:remain) = (x : unionInf xs ys) : pairsInf remain

cubes x y z k = faces + edges + vertices
  where
    faces = (x * y + y * z + z * x) * 2
    edges = (x * 4 + y * 4 + z * 4) * (k - 1)
    vertices = (k - 1) * (k - 2) `div` 2 * 8

counts = joinInf [joinInf [joinInf [[(cubes x y z k, 1) | k <- [1..]] | z <- [y..]] | y <- [x..]] | x <- [1..]]

result126 = head [x | (x, ways) <- counts, ways == 1000]
