module Config.Colors
  ( PrimaryColors (..),
    NormalColors (..),
    BrightColors (..),
    Colors (..),
    dark,
    light,
  )
where

import AesonOptions (options)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

data PrimaryColors = PrimaryColors
  { background :: !Text,
    foreground :: !Text,
    dimForeground :: !(Maybe Text)
  }
  deriving (Show, Generic)

instance Aeson.ToJSON PrimaryColors where
  toJSON = Aeson.genericToJSON options
  toEncoding = Aeson.genericToEncoding options

data CursorColors = CursorColors
  { text :: !Text,
    cursor :: !Text
  }
  deriving (Show, Generic)

instance Aeson.ToJSON CursorColors where
  toJSON = Aeson.genericToJSON options
  toEncoding = Aeson.genericToEncoding options

data ViModeCursorColors = ViModeCursorColors
  { text :: !Text,
    cursor :: !Text
  }
  deriving (Show, Generic)

instance Aeson.ToJSON ViModeCursorColors where
  toJSON = Aeson.genericToJSON options
  toEncoding = Aeson.genericToEncoding options

data SelectionColors = SelectionColors
  { text :: !Text,
    background :: !Text
  }
  deriving (Show, Generic)

instance Aeson.ToJSON SelectionColors where
  toJSON = Aeson.genericToJSON options
  toEncoding = Aeson.genericToEncoding options

data SearchMatchesColors = SearchMatchesColors
  { foreground :: !Text,
    background :: !Text
  }
  deriving (Show, Generic)

instance Aeson.ToJSON SearchMatchesColors where
  toJSON = Aeson.genericToJSON options
  toEncoding = Aeson.genericToEncoding options

data SearchBarColors = SearchBarColors
  { foreground :: !Text,
    background :: !Text
  }
  deriving (Show, Generic)

instance Aeson.ToJSON SearchBarColors where
  toJSON = Aeson.genericToJSON options
  toEncoding = Aeson.genericToEncoding options

data SearchColors = SearchColors
  { matches :: !SearchMatchesColors,
    bar :: !SearchBarColors
  }
  deriving (Show, Generic)

instance Aeson.ToJSON SearchColors where
  toJSON = Aeson.genericToJSON options
  toEncoding = Aeson.genericToEncoding options

data NormalColors = NormalColors
  { black :: !Text,
    red :: !Text,
    green :: !Text,
    yellow :: !Text,
    blue :: !Text,
    magenta :: !Text,
    cyan :: !Text,
    white :: !Text
  }
  deriving (Show, Generic)

instance Aeson.ToJSON NormalColors where
  toJSON = Aeson.genericToJSON options
  toEncoding = Aeson.genericToEncoding options

data BrightColors = BrightColors
  { black :: !Text,
    red :: !Text,
    green :: !Text,
    yellow :: !Text,
    blue :: !Text,
    magenta :: !Text,
    cyan :: !Text,
    white :: !Text
  }
  deriving (Show, Generic)

instance Aeson.ToJSON BrightColors where
  toJSON = Aeson.genericToJSON options
  toEncoding = Aeson.genericToEncoding options

data DimColors = DimColors
  { black :: !Text,
    red :: !Text,
    green :: !Text,
    yellow :: !Text,
    blue :: !Text,
    magenta :: !Text,
    cyan :: !Text,
    white :: !Text
  }
  deriving (Show, Generic)

instance Aeson.ToJSON DimColors where
  toJSON = Aeson.genericToJSON options
  toEncoding = Aeson.genericToEncoding options

data Colors = Colors
  { primary :: !PrimaryColors,
    cursor :: !(Maybe CursorColors),
    viModeCursor :: !(Maybe ViModeCursorColors),
    selection :: !(Maybe SelectionColors),
    search :: !(Maybe SearchColors),
    normal :: !NormalColors,
    bright :: !BrightColors,
    dim :: !(Maybe DimColors)
  }
  deriving (Show, Generic)

instance Aeson.ToJSON Colors where
  toJSON = Aeson.genericToJSON options
  toEncoding = Aeson.genericToEncoding options

-- | Nord (source: https://github.com/eendroroy/alacritty-theme/blob/master/themes/nord.yaml)
dark :: Colors
dark =
  Colors
    { primary =
        PrimaryColors
          { background = "0x2E3440",
            foreground = "0xD8DEE9",
            dimForeground = Just "0xa5abb6"
          },
      cursor =
        Just
          CursorColors
            { text = "0x2e3440",
              cursor = "0xd8dee9"
            },
      viModeCursor =
        Just
          ViModeCursorColors
            { text = "0x2e3440",
              cursor = "0xd8dee9"
            },
      selection =
        Just
          SelectionColors
            { text = "CellForeground",
              background = "0x4c566a"
            },
      search =
        Just
          SearchColors
            { matches =
                SearchMatchesColors
                  { foreground = "CellBackground",
                    background = "0x88c0d0"
                  },
              bar =
                SearchBarColors
                  { background = "0x434c5e",
                    foreground = "0xd8dee9"
                  }
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
          },
      dim =
        Just
          DimColors
            { black = "0x373e4d",
              red = "0x94545d",
              green = "0x809575",
              yellow = "0xb29e75",
              blue = "0x68809a",
              magenta = "0x8c738c",
              cyan = "0x6d96a5",
              white = "0xaeb3bb"
            }
    }

-- | Gruvbox Light (source: https://github.com/eendroroy/alacritty-theme/blob/master/themes/gruvbox_light.yaml)
light :: Colors
light =
  Colors
    { primary =
        PrimaryColors
          { background = "0xf9f5d7",
            foreground = "0x3c3836",
            dimForeground = Nothing
          },
      cursor = Nothing,
      viModeCursor = Nothing,
      selection = Nothing,
      search = Nothing,
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
          },
      dim = Nothing
    }
