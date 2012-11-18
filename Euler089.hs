-- The rules for writing Roman numerals allow for many ways of writing
-- each number (see About Roman Numerals...). However, there is always
-- a "best" way of writing a particular number.

-- For example, the following represent all of the legitimate ways of
-- writing the number sixteen:

-- IIIIIIIIIIIIIIII
-- VIIIIIIIIIII
-- VVIIIIII
-- XIIIIII
-- VVVI
-- XVI

-- The last example being considered the most efficient, as it uses
-- the least number of numerals.

-- The 11K text file, roman.txt (right click and 'Save Link/Target
-- As...'), contains one thousand numbers written in valid, but not
-- necessarily minimal, Roman numerals; that is, they are arranged in
-- descending units and obey the subtractive pair rule (see About
-- Roman Numerals... for the definitive rules for this problem).

-- Find the number of characters saved by writing each of these in
-- their minimal form.

-- Note: You can assume that all the Roman numerals in the file
-- contain no more than four consecutive identical units.

module Euler089 where

import qualified Data.Text as T

reducedLen :: String -> Int
reducedLen input = T.length num - T.length result
  where num = T.pack input
        dcccc = T.pack "DCCCC"
        lxxxx = T.pack "LXXXX"
        viiii = T.pack "VIIII"
        cccc = T.pack "CCCC"
        xxxx = T.pack "XXXX"
        iiii = T.pack "IIII"
        rr = T.pack "RR"
        result = T.replace iiii rr $ T.replace xxxx rr $ T.replace cccc rr $
                 T.replace viiii rr $ T.replace lxxxx rr $ T.replace dcccc rr num

result089 = fmap (sum . map reducedLen . lines) $ readFile "data/roman.txt"
