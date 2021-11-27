module Classifier
    ( classify
    )
where

import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import           Data.Char                      ( isAlpha
                                                , isUpper
                                                )

import           TextKind                       ( TextKind(..) )
import           Sentence                       ( Sentence
                                                , constructFrom
                                                , process
                                                )

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
classifySentence sentence = case (intentionOf sentence, toneOf sentence) of
    (Asking , Yelling) -> YellQuestion
    (Asking , Calm   ) -> Question
    (Telling, Yelling) -> Yell
    (Telling, Calm   ) -> Statement

data Intention = Asking | Telling
data Tone = Yelling | Calm

intentionOf :: Sentence -> Intention
intentionOf =
    process (\(_, ending) -> if ending == '?' then Asking else Telling)

toneOf :: Sentence -> Tone
toneOf = process
    (\(body, ending) ->
        let chars    = T.filter isAlpha (T.snoc body ending)
            nonEmpty = chars /= T.empty
            allUpper = T.all isUpper chars
        in  if nonEmpty && allUpper then Yelling else Calm
    )
