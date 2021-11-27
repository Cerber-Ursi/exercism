module Pangram (isPangram, checkByIterating, checkBySubtracting) where

import Data.Char
import Data.List

-- Proxy function, delegating to one of the implementations below.
-- Both these implementations will pass the last test, terminating ASAP.
isPangram :: String -> Bool
isPangram = checkByIterating


-- Simple implementation, checking explicitly that each letter is in the text
checkByIterating :: String -> Bool
checkByIterating text = all (contains text) ['a'..'z']

contains :: String -> Char -> Bool
contains text letter = letter `elem` (normalize text)


-- Alternative implementation, using the Data.List functions.
-- It is more optimal when the letters in the input string are more or less ordered,
-- and close to the simple implementetion otherwise.
checkBySubtracting :: String -> Bool
checkBySubtracting text = missingLetters text == ""

missingLetters :: String -> String
missingLetters text = (['a'..'z'] \\ (normalize text))


-- Supplementary function, used in both cases
normalize :: String -> String
normalize = map toLower