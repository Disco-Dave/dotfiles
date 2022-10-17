module Shared.Theme.Font
  ( Font (..)
  , toPangoText
  , toPangoString
  , toXftText
  , toXftString
  )
where

import Data.Text (Text)
import Data.Text qualified as Text
import Numeric.Natural (Natural)
import Shared.Utils (showToText)


data Font = Font
  { name :: Text
  , size :: Natural
  }
  deriving (Show, Eq)


toPangoText :: Font -> Text
toPangoText font =
  font.name <> " " <> showToText font.size


toPangoString :: Font -> String
toPangoString =
  Text.unpack . toPangoText


toXftText :: Font -> Text
toXftText font =
  "xft:" <> font.name <> ":size=" <> showToText font.size


toXftString :: Font -> String
toXftString =
  Text.unpack . toXftText
