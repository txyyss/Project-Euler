-- Starting in the top left corner of a 2 * 2 grid, there are 6 routes
-- (without backtracking) to the bottom right corner.

-- How many routes are there through a 20 * 20 grid?

module Euler015 where

routesCount :: Integral a => a -> a -> a
routesCount m n = product [(m + 1)..(m + n)] `div` product [2..n]

result015 = routesCount 20 20
