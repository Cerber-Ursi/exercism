module Bob
    ( responseFor
    )
where

import           Data.Text                      ( Text )
import           TextKind                       ( replyTo )
import           Classifier                     ( classify )

responseFor :: Text -> Text
responseFor = replyTo . classify
