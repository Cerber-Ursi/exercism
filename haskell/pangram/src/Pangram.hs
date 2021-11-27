module Pangram (isPangram, checkByIterating, checkBySubtracting, checkBySet) where

import Data.Char
import Data.List
import Data.Set (isSubsetOf, fromAscList, fromList)

-- Proxy function, delegating to one of the implementations below.
-- Both list-based implementations will pass the last test, terminating ASAP.
isPangram :: String -> Bool
isPangram = checkBySet


-- Simple implementation, checking explicitly that each letter is in the text
checkByIterating :: String -> Bool
checkByIterating text = all (contains text) ['a'..'z']

contains :: String -> Char -> Bool
contains text letter = letter `elem` (normalize text)


-- Alternative implementation, using the Data.List functions.
-- It is more optimal when the letters in the input string are more or less ordered,
-- and close to the simple implementation otherwise.
checkBySubtracting :: String -> Bool
checkBySubtracting text = missingLetters text == ""

missingLetters :: String -> String
missingLetters text = (['a'..'z'] \\ (normalize text))


-- Implementation based on Data.Set.
-- It does not pass the last test, since the second Set has to consume the whole input.
-- Surprisingly, it performs better on "positive" cases and worse on "negative" once.
checkBySet :: String -> Bool
checkBySet = isSubsetOf (fromAscList ['a'..'z']) . fromList . normalize


-- Supplementary function, used in both cases
normalize :: String -> String
normalize = map toLower