-- A hexagonal tile with number 1 is surrounded by a ring of six
-- hexagonal tiles, starting at "12 o'clock" and numbering the tiles 2
-- to 7 in an anti-clockwise direction.

-- New rings are added in the same fashion, with the next rings being
-- numbered 8 to 19, 20 to 37, 38 to 61, and so on. The diagram below
-- shows the first three rings.

-- By finding the difference between tile n and each its six
-- neighbours we shall define PD(n) to be the number of those
-- differences which are prime.

-- For example, working clockwise around tile 8 the differences are
-- 12, 29, 11, 6, 1, and 13. So PD(8) = 3.

-- In the same way, the differences around tile 17 are 1, 17, 16, 1,
-- 11, and 10, hence PD(17) = 2.

-- It can be shown that the maximum value of PD(n) is 3.

-- If all of the tiles for which PD(n) = 3 are listed in ascending
-- order to form a sequence, the 10th tile would be 271.

-- Find the 2000th tile in this sequence.

module Euler128 where

-- detailed analysis can be seen in
-- http://www.mathblog.dk/project-euler-128-which-tiles-in-the-hexagonal-arrangement-have-prime-differences-with-neighbours/

minus :: Ord a => [a] -> [a] -> [a]
minus lx@(x:xs) ly@(y:ys) = case compare x y of 
  LT -> x : minus xs ly
  EQ ->     minus xs ys 
  GT ->     minus lx ys
minus xs _ = xs

primes = 2 : primes'
  where
    primes' = sieve [3,5..] 9 primes'
    sieve (x:xs) q ps@ ~(p:t)
      | x < q     = x : sieve xs q ps
      | otherwise =     sieve (xs `minus` [q, q+2*p..]) (head t^2) t

isPrime 1 = False
isPrime n = helper primes
  where helper (x:xs)
          | x * x > n = True
          | n `mod` x == 0 = False
          | otherwise = helper xs

headOfRing n = 3 * n * (n - 1) + 2
headIsOK n = all isPrime [6 * n - 1, 6 * n + 1, 12 * n + 5]
tailOfRing n = 3 * n * (n + 1) + 1
tailIsOK n = all isPrime [6 * n + 5, 6 * n - 1, 12 * n - 7]

result128 = fst . last . take 2000 . filter ((==True) . snd) $ zip (gen2 headOfRing tailOfRing) (gen2 headIsOK tailIsOK)
  where gen2 f g = concatMap (\n -> [f n, g n]) [1..]
