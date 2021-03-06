-- The sequence of triangle numbers is generated by adding the natural
-- numbers. So the 7th triangle number would be 1 + 2 + 3 + 4 + 5 + 6
-- + 7 = 28. The first ten terms would be:

-- 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

-- Let us list the factors of the first seven triangle numbers:

--  1: 1
--  3: 1,3
--  6: 1,2,3,6
-- 10: 1,2,5,10
-- 15: 1,3,5,15
-- 21: 1,3,7,21
-- 28: 1,2,4,7,14,28

-- We can see that 28 is the first triangle number to have over five
-- divisors.

-- What is the value of the first triangle number to have over five
-- hundred divisors?

module Euler012 where

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

numberOfDivisors :: Int -> Int
numberOfDivisors n = tryDivide n 1 primes
  where tryDivide 1 count _ = count
        tryDivide remain count (x:xs)
          | x * x > n = 2 * count
          | remain `mod` x /= 0 = tryDivide remain count xs
          | otherwise = tryDivide remain' ((2 + repeats) * count) xs
          where (remain', repeats) = tryRemove (remain `div` x) 0 x
                tryRemove a c b
                  | a `mod` b /= 0 = (a, c)
                  | otherwise = tryRemove (a `div` b) (c + 1) b

countOfDivisors = map numberOfDivisors [0..]

countDivisorsOfTriIndex :: Int -> Int
countDivisorsOfTriIndex n
  | odd n = (countOfDivisors !! n) * (countOfDivisors !! ((n+1) `div` 2))
  | otherwise = (countOfDivisors !! (n `div` 2)) * (countOfDivisors !! (n+1))

index = fst $ head $ filter (\x -> snd x > 500) $ map (\x -> (x, countDivisorsOfTriIndex x)) [1..]

result012 = index * (index + 1) `div` 2
