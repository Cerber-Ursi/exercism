{-# LANGUAGE OverloadedStrings #-}
module TextKind
    ( TextKind(..)
    , replyTo
    )
where

import           Data.Text                      ( Text )

data TextKind = Question | Yell | YellQuestion | Statement | Silence

replyTo :: TextKind -> Text
replyTo Question     = "Sure."
replyTo Yell         = "Whoa, chill out!"
replyTo YellQuestion = "Calm down, I know what I'm doing!"
replyTo Statement    = "Whatever."
replyTo Silence      = "Fine. Be that way!"
