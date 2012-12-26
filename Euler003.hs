-- The prime factors of 13195 are 5, 7, 13 and 29.

-- What is the largest prime factor of the number 600851475143 ?

module Euler003 where

-- a slow prime test version
minus :: Ord a => [a] -> [a] -> [a]
minus lx@(x:xs) ly@(y:ys) = case compare x y of 
  LT -> x : minus xs ly
  EQ ->     minus xs ys 
  GT ->     minus lx ys
minus xs _ = xs

primesTo :: Int -> [Int]
primesTo m = 2 : sieve [3,5..m]  where
  sieve (p:xs) 
    | p*p > m   = p : xs
    | otherwise = p : sieve (xs `minus` [p*p, p*p+2*p..])

maxPrimeFactor :: Int -> Int
maxPrimeFactor x = testDiv primes
  where primes = reverse $ primesTo $ floor $ sqrt $ fromIntegral x
        testDiv (y:ys) 
          | x `mod` y == 0 = y
          | otherwise = testDiv ys

result003 = maxPrimeFactor 600851475143

-- faster trivial test version

findFactors :: Int -> [Int]
findFactors x = testDiv x $ 2:[3,5..(floor $ sqrt $ fromIntegral x)]
  where testDiv target (y:ys)
          | target `mod` y == 0 = y : testDiv (remove target y) ys
          | otherwise = testDiv target ys
        testDiv _ [] = []
        remove m n
          | m `mod` n == 0 = remove (m `div` n) n
          | otherwise = m

result003' = last $ findFactors 600851475143
