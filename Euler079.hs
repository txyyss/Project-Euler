-- A common security method used for online banking is to ask the user
-- for three random characters from a passcode. For example, if the
-- passcode was 531278, they may ask for the 2nd, 3rd, and 5th
-- characters; the expected reply would be: 317.

-- The text file, keylog.txt, contains fifty successful login
-- attempts.

-- Given that the three characters are always asked for in order,
-- analyse the file so as to determine the shortest possible secret
-- passcode of unknown length.

module Euler079 where

import Data.Graph
import Data.List
import Data.Char

findPassword :: String -> String
findPassword content = sortResult `intersect` candidate
  where edges = concatMap ((\[x,y,z]-> [(x,y),(y,z)]). map digitToInt) $ lines content
        sortResult = map intToDigit . topSort $ buildG (0,9) edges
        candidate = "0123456789" `intersect` content

result079 = fmap findPassword $ readFile "data/keylog.txt"
