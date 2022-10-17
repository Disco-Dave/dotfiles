module Shared.Theme.Font
  ( Font (..)
  , toPangoText
  , toPangoString
  , toXftText
  , toXftString
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import Shared.Utils (showToText)


data Font = Font
  { name :: Text
  , size :: Natural
  }
  deriving (Show, Eq)


toPangoText :: Font -> Text
toPangoText Font{..} =
  name <> " " <> showToText size


toPangoString :: Font -> String
toPangoString =
  Text.unpack . toPangoText


toXftText :: Font -> Text
toXftText Font{..} =
  "xft:" <> name <> ":size=" <> showToText size


toXftString :: Font -> String
toXftString =
  Text.unpack . toXftText
