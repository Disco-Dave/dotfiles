module Config.Font
  ( FontFamily (..),
    Font (..),
  )
where

import AesonOptions (options)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

newtype FontFamily = FontFamily
  { family :: Text
  }
  deriving (Show, Generic)

instance Aeson.ToJSON FontFamily where
  toJSON = Aeson.genericToJSON options
  toEncoding = Aeson.genericToEncoding options

data Font = Font
  { normal :: !FontFamily,
    size :: !Double
  }
  deriving (Show, Generic)

instance Aeson.ToJSON Font where
  toJSON = Aeson.genericToJSON options
  toEncoding = Aeson.genericToEncoding options
