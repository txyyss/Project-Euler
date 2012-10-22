-- The fraction 49/98 is a curious fraction, as an inexperienced
-- mathematician in attempting to simplify it may incorrectly believe
-- that 49/98 = 4/8, which is correct, is obtained by cancelling the
-- 9s.

-- We shall consider fractions like, 30/50 = 3/5, to be trivial
-- examples.

-- There are exactly four non-trivial examples of this type of
-- fraction, less than one in value, and containing two digits in the
-- numerator and denominator.

-- If the product of these four fractions is given in its lowest
-- common terms, find the value of the denominator.

module Euler033 where

isNonTrivialFraction :: (Int, Int) -> Bool
isNonTrivialFraction xy@(x, y)
  | (gcd x y) `mod` 10 == 0 = False
  | gcd x y == 1 = False
  | aX * rY == rX * aY = True
  | otherwise = False
  where (aX, aY) = actValue xy
        (rX, rY) = removeCommon xy

actValue :: (Int, Int) -> (Int, Int)
actValue (x, y) = (x `div` g, y `div` g)
  where g = gcd x y

removeCommon :: (Int, Int) -> (Int, Int)
removeCommon (x, y) 
  | tenX == tenY = (digX, digY)
  | tenX == digY = (digX, tenY)
  | digX == tenY = (tenX, digY)
  | digX == digY = (tenX, tenY)
  | otherwise = (0,1)
  where tenX = x `div` 10
        digX = x `mod` 10
        tenY = y `div` 10
        digY = y `mod` 10

nonTrivials = map actValue $ filter isNonTrivialFraction [(x, y) | x <- [10..99], y<-[(x+1)..99]]

result033 = d `div` gcd n d
  where n = product $ map fst nonTrivials
        d = product $ map snd nonTrivials
