-- You are given the following information, but you may prefer to do
-- some research for yourself.

-- 1 Jan 1900 was a Monday.
-- Thirty days has September,
-- April, June and November.
-- All the rest have thirty-one,
-- Saving February alone,
-- Which has twenty-eight, rain or shine.
-- And on leap years, twenty-nine.

-- A leap year occurs on any year evenly divisible by 4, but not on a
-- century unless it is divisible by 400.

-- How many Sundays fell on the first of the month during the
-- twentieth century (1 Jan 1901 to 31 Dec 2000)?

module Euler019 where

daysOfMonth = [0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

isLeapYear year
  | year `mod` 100 == 0 = year `mod` 400 == 0
  | otherwise = year `mod` 4 == 0
                
finalDay year 2 = if isLeapYear year then 29 else 28
finalDay year month = daysOfMonth !! month

nextDay :: (Int, Int, Int, Int) -> (Int, Int, Int, Int)
nextDay (year, month, day, dayOfWeek)
  | month == 12 && day == 31 = (year + 1, 1, 1, nextDayOfWeek)
  | day == finalDay year month = (year, month + 1, 1, nextDayOfWeek)
  | otherwise = (year, month, day + 1, nextDayOfWeek)
  where nextDayOfWeek = (dayOfWeek + 1) `mod` 7

thisCentery = filter (\(y, _, _, _) -> y >= 1901 && y <= 2000) $ take 37000 $ iterate nextDay (1900,1,1,1)

result019 = length $ filter (\(_, _, d, w) -> d == 1 && w == 0) thisCentery 
