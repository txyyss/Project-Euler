-- By replacing the 1st digit of *3, it turns out that six of the nine
-- possible values: 13, 23, 43, 53, 73, and 83, are all prime.

-- By replacing the 3rd and 4th digits of 56**3 with the same digit,
-- this 5-digit number is the first example having seven primes among
-- the ten generated numbers, yielding the family: 56003, 56113,
-- 56333, 56443, 56663, 56773, and 56993. Consequently 56003, being
-- the first member of this family, is the smallest prime with this
-- property.

-- Find the smallest prime which, by replacing part of the number (not
-- necessarily adjacent digits) with the same digit, is part of an
-- eight prime value family.

module Euler051 where

import Data.Char (digitToInt)

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

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = helper primes
  where helper (x:xs)
          | x * x > n = True
          | n `mod` x == 0 = False
          | otherwise = helper xs

splitToDigits :: Show a => a -> [Int]
splitToDigits n = map digitToInt $ show n
      
splitNum :: [Int] -> [([Int], [Int])]
splitNum = splitEach [] []
  where splitEach existed _ [_] = existed
        splitEach existed already l@(x:xs)
          | x <= 2 = splitEach ((already, l):existed) (already ++ [x]) xs
          | otherwise = splitEach existed (already ++ [x]) xs

segment :: Eq a => a -> [a] -> [[[a]]]
segment m ns = segHelper [[ns]] [] [] ns
  where segHelper result _ _ [_] = result
        segHelper result existed already (x:xs)
          | x == m = segHelper ((newExisted ++ [xs]):result) newExisted [] xs
          | otherwise = segHelper result existed (already ++ [x]) xs
          where newExisted = existed ++ [already]

test :: ([Int], [Int]) -> [[Int]]
test (a, b) = map (\x -> map (\n -> foldl (composeNum n) numA x) [start..9]) segB
  where numA = if null a then 0 else foldl1 helper a
        start = head b
        segB = segment start (tail b)
        composeNum n preNum = foldl helper (helper preNum n)
        helper x y = x * 10 + y

isWanted :: [Int] -> Bool
isWanted = any ((\ x -> length x >= 8) . filter isPrime) . concatMap test . splitNum

result051 = foldl1 (\x y -> x * 10 + y) . head . filter isWanted  . filter (any (<=2) . init) $ map splitToDigits primes
