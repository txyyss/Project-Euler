-- Su Doku (Japanese meaning number place) is the name given to a
-- popular puzzle concept. Its origin is unclear, but credit must be
-- attributed to Leonhard Euler who invented a similar, and much more
-- difficult, puzzle idea called Latin Squares. The objective of Su
-- Doku puzzles, however, is to replace the blanks (or zeros) in a 9
-- by 9 grid in such that each row, column, and 3 by 3 box contains
-- each of the digits 1 to 9. Below is an example of a typical
-- starting puzzle grid and its solution grid.

-- 0 0 3  0 2 0  6 0 0         4 8 3  9 2 1  6 5 7
-- 9 0 0  3 0 5  0 0 1         9 6 7  3 4 5  8 2 1
-- 0 0 1  8 0 6  4 0 0         2 5 1  8 7 6  4 9 3
                                               
-- 0 0 8  1 0 2  9 0 0         5 4 8  1 3 2  9 7 6
-- 7 0 0  0 0 0  0 0 8         7 2 9  5 6 4  1 3 8
-- 0 0 6  7 0 8  2 0 0         1 3 6  7 9 8  2 4 5
                                               
-- 0 0 2  6 0 9  5 0 0         3 7 2  6 8 9  5 1 4
-- 8 0 0  2 0 3  0 0 9         8 1 4  2 5 3  7 6 9
-- 0 0 5  0 1 0  3 0 0         6 9 5  4 1 7  3 8 2

-- A well constructed Su Doku puzzle has a unique solution and can be
-- solved by logic, although it may be necessary to employ "guess and
-- test" methods in order to eliminate options (there is much
-- contested opinion over this). The complexity of the search
-- determines the difficulty of the puzzle; the example above is
-- considered easy because it can be solved by straight forward direct
-- deduction.

-- The 6K text file, sudoku.txt (right click and 'Save Link/Target
-- As...'), contains fifty different Su Doku puzzles ranging in
-- difficulty, but all with unique solutions (the first puzzle in the
-- file is the example above).

-- By solving all fifty puzzles find the sum of the 3-digit numbers
-- found in the top left corner of each solution grid; for example,
-- 483 is the 3-digit number found in the top left corner of the
-- solution grid above.

module Euler096 where

-- I use the code in http://www.cs.nott.ac.uk/~gmh/sudoku.lhs

import Data.List
import Data.Char

-- Basic Data Structure

type Grid = Matrix Value
type Matrix a = [Row a]
type Row a = [a]
type Value = Int

-- Simple definition

boxSize :: Int
boxSize = 3

values :: [Value]
values = [1..9]

empty :: Value -> Bool
empty = (==0)

single :: [a] -> Bool
single [_] = True
single _ = False

-- Extracting rows, columns and boxes

rows :: Matrix a -> [Row a]
rows = id

cols :: Matrix a -> [Row a]
cols = transpose

boxes :: Matrix a -> [Row a]
boxes = unpack . map cols . pack
  where pack = split . map split
        split = chop boxSize
        unpack = map concat . concat

chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = take n xs : chop n (drop n xs)

-- Validity checking

noDups :: Eq a => [a] -> Bool
noDups [] = True
noDups (x:xs) = notElem x xs && noDups xs

-- Solver

type Choices = [Value]

choices :: Grid -> Matrix Choices
choices = map (map choice)
  where choice v
          | empty v = values
          | otherwise = [v]

cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs:xss) = [y:ys | y <- xs, ys <- cp xss]

collapse :: Matrix [a] -> [Matrix a]
collapse = cp . map cp

-- Pruning the search space

prune :: Matrix Choices -> Matrix Choices
prune = pruneBy boxes . pruneBy cols . pruneBy rows
  where pruneBy f = f . map reduce . f

reduce :: Row Choices -> Row Choices
reduce xss = [xs `minus` singles | xs <- xss]
  where singles = concat (filter single xss)

minus :: Choices -> Choices -> Choices
xs `minus` ys = if single xs then xs else xs \\ ys

-- Properties of matrices

complete :: Matrix Choices -> Bool
complete = all (all single)

void :: Matrix Choices -> Bool
void = any (any null)

safe :: Matrix Choices -> Bool
safe cm = all consistent (rows cm) &&
          all consistent (cols cm) &&
          all consistent (boxes cm)

consistent :: Row Choices -> Bool
consistent = noDups . concat . filter single

blocked :: Matrix Choices -> Bool
blocked m = void m || not (safe m)

-- Making choices one at a time

search :: Matrix Choices -> [Grid]
search m
  | blocked m = []
  | complete m = collapse m
  | otherwise = [g | m' <- expand m, g <- search (prune m')]

expand :: Matrix Choices -> [Matrix Choices]
expand m = [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
  where (rows1,row:rows2) = break (any (not . single)) m
        (row1,cs:row2)    = span single row

solveSudoku :: Grid -> [Grid]
solveSudoku = search . prune . choices

toGrid :: [String] -> Grid
toGrid = map (map digitToInt)

result096 = fmap (sum . map solve . chop 10 . lines) $ readFile "data/sudoku.txt"
  where solve = foldl1 (\x y -> x* 10 + y) . take 3 . head . head . solveSudoku . toGrid . tail
