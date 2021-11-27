module LeapYear (isLeapYear, isLeapYearRem, isLeapYearRemReversed) where

isLeapYear :: Integer -> Bool
isLeapYear year
    | year `mod` 400 == 0 = True
    | year `mod` 100 == 0 = False
    | year `mod` 4 == 0   = True
    | otherwise           = False

isLeapYearRem :: Integer -> Bool
isLeapYearRem year
    | year `rem` 400 == 0 = True
    | year `rem` 100 == 0 = False
    | year `rem` 4 == 0   = True
    | otherwise           = False

isLeapYearRemReversed :: Integer -> Bool
isLeapYearRemReversed year
    | year `rem`   4 /= 0 = False
    | year `rem` 100 /= 0 = True
    | year `rem` 400 /= 0 = False
    | otherwise           = True