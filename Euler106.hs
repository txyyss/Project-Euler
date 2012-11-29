-- Let S(A) represent the sum of elements in set A of size n. We shall
-- call it a special sum set if for any two non-empty disjoint
-- subsets, B and C, the following properties are true:

-- S(B) != S(C); that is, sums of subsets cannot be equal.
-- If B contains more elements than C then S(B) > S(C).

-- For this problem we shall assume that a given set contains n
-- strictly increasing elements and it already satisfies the second
-- rule.

-- Surprisingly, out of the 25 possible subset pairs that can be
-- obtained from a set for which n = 4, only 1 of these pairs need to
-- be tested for equality (first rule). Similarly, when n = 7, only 70
-- out of the 966 subset pairs need to be tested.

-- For n = 12, how many of the 261625 subset pairs that can be
-- obtained need to be tested for equality?

-- NOTE: This problem is related to problems 103 and 105.

module Euler106 where

-- see analysis here:
-- http://www.mathblog.dk/project-euler-106-minimum-comparisons-special-sum-sets/

choose :: Int -> Int -> Int
choose m n = product [(n+1)..m] `div` product [2..(m-n)]

catalan :: Int -> Int
catalan n = choose (2*n) n `div` (n+1) 

needCheck :: Int -> Int -> Int
needCheck n s = choose n s * choose (n-s) s `div` 2 - catalan s * choose n (2*s)

result106 = sum $ map (needCheck 12) [2..6]
