-- The cube, 41063625 (345^3), can be permuted to produce two other
-- cubes: 56623104 (384^3) and 66430125 (405^3). In fact, 41063625 is
-- the smallest cube which has exactly three permutations of its
-- digits which are also cube.

-- Find the smallest cube for which exactly five permutations of its
-- digits are cube.

module Euler062 where

import qualified Data.Map as Map
import Data.List

search :: Map.Map String [Integer] -> [Integer] -> Int -> Integer
search numMap (x:xs) limit = helper (Map.lookup key numMap)
  where key = sort $ show x
        helper Nothing = search (Map.insert key [x] numMap) xs limit
        helper (Just ls)
          | length ls == (limit-1) = minimum (x:ls)
          | otherwise = search (Map.insert key (x:ls) numMap) xs limit

result062 = search Map.empty (map (^3) [1..]) 5
