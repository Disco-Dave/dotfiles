module Config.Colors where

import qualified Data.Aeson as Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

data PrimaryColors = PrimaryColors
  { background :: Text,
    foreground :: Text
  }
  deriving (Show, Generic)

instance Aeson.ToJSON PrimaryColors

data NormalColors = NormalColors
  { black :: Text,
    red :: Text,
    green :: Text,
    yellow :: Text,
    blue :: Text,
    magenta :: Text,
    cyan :: Text,
    white :: Text
  }
  deriving (Show, Generic)

instance Aeson.ToJSON NormalColors

data BrightColors = BrightColors
  { black :: Text,
    red :: Text,
    green :: Text,
    yellow :: Text,
    blue :: Text,
    magenta :: Text,
    cyan :: Text,
    white :: Text
  }
  deriving (Show, Generic)

instance Aeson.ToJSON BrightColors

data Colors = Colors
  { primary :: PrimaryColors,
    normal :: NormalColors,
    bright :: BrightColors
  }
  deriving (Show, Generic)

instance Aeson.ToJSON Colors
