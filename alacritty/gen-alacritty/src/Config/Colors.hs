module Config.Colors
  ( PrimaryColors (..),
    NormalColors (..),
    BrightColors (..),
    Colors (..),
    dark,
    light,
  )
where

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

-- | Nord (source: https://github.com/eendroroy/alacritty-theme/blob/master/themes/nord.yaml)
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

-- | Gruvbox Light (source: https://github.com/eendroroy/alacritty-theme/blob/master/themes/gruvbox_light.yaml)
light :: Colors
light =
  Colors
    { primary =
        PrimaryColors
          { background = "0xf9f5d7",
            foreground = "0x3c3836"
          },
      normal =
        NormalColors
          { black = "0xfbf1c7",
            red = "0xcc241d",
            green = "0x98971a",
            yellow = "0xd79921",
            blue = "0x458588",
            magenta = "0xb16286",
            cyan = "0x689d6a",
            white = "0x7c6f64"
          },
      bright =
        BrightColors
          { black = "0x928374",
            red = "0x9d0006",
            green = "0x79740e",
            yellow = "0xb57614",
            blue = "0x076678",
            magenta = "0x8f3f71",
            cyan = "0x427b58",
            white = "0x3c3836"
          }
    }
