-- If the numbers 1 to 5 are written out in words: one, two, three,
-- four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in
-- total.

-- If all the numbers from 1 to 1000 (one thousand) inclusive were
-- written out in words, how many letters would be used?

-- NOTE: Do not count spaces or hyphens. For example, 342 (three
-- hundred and forty-two) contains 23 letters and 115 (one hundred and
-- fifteen) contains 20 letters. The use of "and" when writing out
-- numbers is in compliance with British usage.

module Euler017 where

intToEnglish :: Int -> String
intToEnglish n
  | n >= 1000 = "onethousand"
  | n >= 100 = if n `mod` 100 == 0 then hundredPart n else concat [hundredPart n, "and", intToEnglish (n `mod` 100)]
  | n < 20 = under20 !! n
  | otherwise = if n `mod` 10 == 0 then tenPart n else concat [tenPart n, intToEnglish (n `mod` 10)]
  where hundredPart m = concat [under20 !! (m `div` 100), "hundred"]
        tenPart m = above20 !! (m `div` 10)
        under20 = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", 
                   "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
        above20 = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]
        
result017 = sum $ map (length . intToEnglish) [1..1000]
