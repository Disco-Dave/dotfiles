module XMonad.Local.Theme.Font (
  Font (..),
  toXftText,
  toXftString,
) where

import Data.Text (Text)
import qualified Data.Text as Text
import Numeric.Natural (Natural)

data Font = Font
  { fontFamily :: Text
  , fontSize :: Natural
  }
  deriving (Show, Eq)

toXftText :: Font -> Text
toXftText Font{..} =
  "xft:" <> fontFamily <> ":size=" <> Text.pack (show fontSize)

toXftString :: Font -> String
toXftString =
  Text.unpack . toXftText
