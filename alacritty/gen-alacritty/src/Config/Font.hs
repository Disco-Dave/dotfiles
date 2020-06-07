module Config.Font where

import qualified Data.Aeson as Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

newtype FontFamily = FontFamily {familyName :: Text} deriving (Show, Generic)

instance Aeson.ToJSON FontFamily where
  toJSON (FontFamily familyName) =
    Aeson.object ["family" Aeson..= familyName]
  toEncoding (FontFamily familyName) =
    Aeson.pairs $ "family" Aeson..= familyName

data Font = Font {normal :: FontFamily, size :: Double} deriving (Show, Generic)

instance Aeson.ToJSON Font where
  toJSON Font {..} =
    Aeson.object
      [ "normal" Aeson..= normal,
        "size" Aeson..= size
      ]
  toEncoding Font {..} =
    Aeson.pairs $
      "normal" Aeson..= normal
        <> "size" Aeson..= size
