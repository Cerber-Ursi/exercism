module Sentence(Sentence, constructFrom, process)
where

import qualified Data.Text as T
import           Data.Text                      ( Text )

-- A sentence object.
--
-- Its main property is the fact that underlying Text is non-empty.
-- By convention, it holds the last Char of original Text separately -
-- this makes it a bit easier to check some of the sentence's properties.
data Sentence = Sentence Text Char

-- Constructor for Sentence objects. 
--
-- It will first strip all trailing and leading spaces from input,
-- since they don't have any semantic meaning, and then check if
-- the result is non-empty.
constructFrom :: Text -> Maybe Sentence
constructFrom text = do
    (body, ending) <- T.unsnoc $ T.strip text
    Just (Sentence body ending)

-- Wrapper to process the Sentence with arbitrary mapper.
--
-- Two implementation details:
-- 1. `Sentence` is used as last argument, so that it can be omitted
-- in common cases (when the calling function is :: Sentence -> a).
-- 2. Mapper takes Sentence components as tuple, so that it is
-- a bit easier to pass the lambda function there.
process :: ((Text, Char) -> a) -> Sentence -> a
process mapper (Sentence text char) = mapper (text, char)