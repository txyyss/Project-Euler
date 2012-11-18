-- Each of the six faces on a cube has a different digit (0 to 9)
-- written on it; the same is done to a second cube. By placing the
-- two cubes side-by-side in different positions we can form a variety
-- of 2-digit numbers.

-- For example, the square number 64 could be formed:

-- In fact, by carefully choosing the digits on both cubes it is
-- possible to display all of the square numbers below one-hundred:
-- 01, 04, 09, 16, 25, 36, 49, 64, and 81.

-- For example, one way this can be achieved is by placing {0, 5, 6,
-- 7, 8, 9} on one cube and {1, 2, 3, 4, 8, 9} on the other cube.

-- However, for this problem we shall allow the 6 or 9 to be turned
-- upside-down so that an arrangement like {0, 5, 6, 7, 8, 9} and {1,
-- 2, 3, 4, 6, 7} allows for all nine square numbers to be displayed;
-- otherwise it would be impossible to obtain 09.

-- In determining a distinct arrangement we are interested in the
-- digits on each cube, not the order.

-- {1, 2, 3, 4, 5, 6} is equivalent to {3, 6, 4, 1, 2, 5}
-- {1, 2, 3, 4, 5, 6} is distinct from {1, 2, 3, 4, 5, 9}

-- But because we are allowing 6 and 9 to be reversed, the two
-- distinct sets in the last example both represent the extended set
-- {1, 2, 3, 4, 5, 6, 9} for the purpose of forming 2-digit numbers.

-- How many distinct arrangements of the two cubes allow for all of
-- the square numbers to be displayed?

module Euler090 where

combinations :: Int -> [a] -> [[a]]
combinations _ [] = []
combinations 1 xs = map (\x -> [x]) xs
combinations n (x:xs) = combinations n xs ++ (map (x:) $ combinations (n-1) xs)

isInDice :: Int -> [Int] -> Bool
isInDice 6 a = elem 6 a || elem 9 a
isInDice 9 a = elem 6 a || elem 9 a
isInDice i a = elem i a

canDisplay :: [Int] -> [Int] -> Int -> Bool
canDisplay a b i = (isInDice i1 a && isInDice i2 b)  || (isInDice i1 b && isInDice i2 a)
  where i1 = i `div` 10
        i2 = i `mod` 10

findDices :: ([Int],[Int]) -> Bool
findDices (a,b) = all (canDisplay a b) [1,4,9,16,25,36,49,64,81]

allDices = combinations 6 [0..9]

result090 = (`div` 2) $ length $ filter findDices [(a,b)| a<-allDices, b<-allDices]
