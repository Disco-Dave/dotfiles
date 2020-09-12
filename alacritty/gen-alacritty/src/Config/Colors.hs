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

dark :: Colors
dark =
  Colors
    { primary =
        PrimaryColors
          { background = "0x2E3440",
            foreground = "0xD8DEE9"
          },
      normal =
        NormalColors
          { black = "0x3B4252",
            red = "0xBF616A",
            green = "0xA3BE8C",
            yellow = "0xEBCB8B",
            blue = "0x81A1C1",
            magenta = "0xB48EAD",
            cyan = "0x88C0D0",
            white = "0xE5E9F0"
          },
      bright =
        BrightColors
          { black = "0x4C566A",
            red = "0xBF616A",
            green = "0xA3BE8C",
            yellow = "0xEBCB8B",
            blue = "0x81A1C1",
            magenta = "0xB48EAD",
            cyan = "0x8FBCBB",
            white = "0xECEFF4"
          }
    }

light :: Colors
light =
  Colors
    { primary =
        PrimaryColors
          { background = "0xf1f1f1",
            foreground = "0x424242"
          },
      normal =
        NormalColors
          { black = "0x212121",
            red = "0xc30771",
            green = "0x10a778",
            yellow = "0xa89c14",
            blue = "0x008ec4",
            magenta = "0x523c79",
            cyan = "0x20a5ba",
            white = "0xe0e0e0"
          },
      bright =
        BrightColors
          { black = "0x212121",
            red = "0xfb007a",
            green = "0x5fd7af",
            yellow = "0xf3e430",
            blue = "0x20bbfc",
            magenta = "0x6855de",
            cyan = "0x4fb8cc",
            white = "0xf1f1f1"
          }
    }
