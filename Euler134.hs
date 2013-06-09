-- Consider the consecutive primes p₁ = 19 and p₂ = 23. It can be
-- verified that 1219 is the smallest number such that the last digits
-- are formed by p₁ whilst also being divisible by p₂.

-- In fact, with the exception of p₁ = 3 and p₂ = 5, for every pair of
-- consecutive primes, p₂ > p₁, there exist values of n for which the
-- last digits are formed by p₁ and n is divisible by p₂. Let S be the
-- smallest of these values of n.

-- Find ∑S for every pair of consecutive primes with 5 ≤ p₁ ≤ 1000000.

module Euler134 where

extGCD a 0 = (1, 0)
extGCD a b = (t, s - q * t)
  where q = a `div` b
        r = a `mod` b
        (s, t) = extGCD b r

solveLinearCongruence a b n
  | b `mod` d /= 0 = error "No Solution"
  | otherwise = (r * b `div` d, n `div` d)
  where d = gcd a n
        (r, s) = extGCD a n

pow10Count n = helper n 1
  where helper 0 x = x
        helper i x = helper (i `div` 10) (x * 10)

miniPosSolve a b n = if modN < 0 then modN + n else modN
  where (x, d) = solveLinearCongruence a b n
        modN = x `mod` n
        
getS (p1, p2) = pow10 * sol + p1
  where pow10 = pow10Count p1
        sol = miniPosSolve pow10 (p2 - p1) p2

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

result134 = sum . map getS . takeWhile ((<=1000000) . fst) . getPair $ dropWhile (<5) primes
  where getPair ls = zip ls (tail ls)
