{-# LANGUAGE OverloadedStrings #-}
module Bob (responseFor) where

import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Char                      ( isAlpha
                                                , isUpper
                                                )

responseFor :: Text -> Text
responseFor = replyTo . classify

data TextKind = Question | Shout | ShoutQuestion | Simple | Silence

data Sentence = Sentence Text Char

constructFrom :: Text -> Maybe Sentence
constructFrom text = do
    (body, ending) <- T.unsnoc $ T.strip text
    Just (Sentence body ending)

replyTo :: TextKind -> Text
replyTo Question = "Sure."
replyTo Shout = "Whoa, chill out!"
replyTo ShoutQuestion = "Calm down, I know what I'm doing!"
replyTo Simple = "Whatever."
replyTo Silence = "Fine. Be that way!"

classify :: Text -> TextKind
classify text = mapOrDefault (constructFrom text) classifySentence Silence
-- Equivalent code:
-- classify text = case constructFrom text of
--     Just sentence -> classifySentence sentence
--     Nothing -> Silence

-- A rearrangement of the standard "maybe" function.
-- It does not have much sense, aside from letting me write the code above
-- in more easily understandable way.
mapOrDefault :: Maybe a -> (a -> b) -> b -> b
mapOrDefault input mapper def = maybe def mapper input

classifySentence :: Sentence -> TextKind
classifySentence sentence = case (isQuestion sentence, isShout sentence) of
    (True, True) -> ShoutQuestion
    (True, False) -> Question
    (False, True) -> Shout
    (False, False) -> Simple

isQuestion :: Sentence -> Bool
isQuestion (Sentence _ ending) = ending == '?'

isShout :: Sentence -> Bool
isShout (Sentence body ending) =
    let isShoutChar c = not (isAlpha c) || isUpper c
    in (T.any isAlpha body || isAlpha ending) && T.all isShoutChar body && isShoutChar ending
