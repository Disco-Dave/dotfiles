module Config.Font where

import qualified Data.Aeson as Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

newtype FontFamily = FontFamily
  { family :: Text
  }
  deriving (Show, Generic)

instance Aeson.ToJSON FontFamily

data Font = Font
  { normal :: FontFamily,
    size :: Double
  }
  deriving (Show, Generic)

instance Aeson.ToJSON Font
