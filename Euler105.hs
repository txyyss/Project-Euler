-- Let S(A) represent the sum of elements in set A of size n. We shall
-- call it a special sum set if for any two non-empty disjoint
-- subsets, B and C, the following properties are true:

-- S(B) != S(C); that is, sums of subsets cannot be equal.
-- If B contains more elements than C then S(B) > S(C).

-- For example, {81, 88, 75, 42, 87, 84, 86, 65} is not a special sum
-- set because 65 + 87 + 88 = 75 + 81 + 84, whereas {157, 150, 164,
-- 119, 79, 159, 161, 139, 158} satisfies both rules for all possible
-- subset pair combinations and S(A) = 1286.

-- Using sets.txt (right click and "Save Link/Target As..."), a 4K
-- text file with one-hundred sets containing seven to twelve elements
-- (the two examples given above are the first two sets in the file),
-- identify all the special sum sets, A1, A2, ..., Ak, and find the
-- value of S(A1) + S(A2) + ... + S(Ak).

-- NOTE: This problem is related to problems 103 and 106.

module Euler105 where

import Data.List
import Data.Ord

noEqualSubset :: [Int] -> Bool
noEqualSubset = noEqualList . sortBy (comparing sum) . subsequences
  where noEqualList [x,y] = helper x y
        noEqualList (x:y:ys)
          | helper x y = noEqualList (y:ys)
          | otherwise = False
        helper xs ys
          | sum xs == sum ys = intersect xs ys /= []
          | otherwise = True

satisfyMore :: [Int] -> Bool
satisfyMore ls = all helper [1..len]
  where totalLen = length ls
        len = (totalLen - 1) `div` 2
        helper n = firstSum > lastSum
          where firstSum = sum $ take (n+1) ls
                lastSum = sum $ drop (totalLen - n) ls

toList :: String -> [Int]
toList s = sort $ read $ "["++s++"]"

result105 = fmap (sum . map sum . filter (\x->noEqualSubset x && satisfyMore x) . map toList . lines) $ readFile "data/sets.txt"
