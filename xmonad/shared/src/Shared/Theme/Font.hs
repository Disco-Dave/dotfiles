module Shared.Theme.Font
  ( Font (..)
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


toXftText :: Font -> Text
toXftText font =
  "xft:" <> font.name <> ":size=" <> showToText font.size


toXftString :: Font -> String
toXftString =
  Text.unpack . toXftText
