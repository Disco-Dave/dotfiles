module Config.Colors where

import qualified Data.Aeson as Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

data PrimaryColors
  = PrimaryColors
      { primaryBackground :: Text,
        primaryForeground :: Text
      }
  deriving (Show, Generic)

instance Aeson.ToJSON PrimaryColors where
  toJSON PrimaryColors {..} =
    Aeson.object
      [ "background" Aeson..= primaryBackground,
        "foreground" Aeson..= primaryForeground
      ]
  toEncoding PrimaryColors {..} =
    Aeson.pairs $
      "background" Aeson..= primaryBackground
        <> "foreground" Aeson..= primaryForeground

data NormalColors
  = NormalColors
      { normalBlack :: Text,
        normalRed :: Text,
        normalGreen :: Text,
        normalYellow :: Text,
        normalBlue :: Text,
        normalMagenta :: Text,
        normalCyan :: Text,
        normalWhite :: Text
      }
  deriving (Show, Generic)

instance Aeson.ToJSON NormalColors where
  toJSON NormalColors {..} =
    Aeson.object
      [ "black" Aeson..= normalBlack,
        "red" Aeson..= normalRed,
        "green" Aeson..= normalGreen,
        "yellow" Aeson..= normalYellow,
        "blue" Aeson..= normalBlue,
        "magenta" Aeson..= normalMagenta,
        "cyan" Aeson..= normalCyan,
        "white" Aeson..= normalWhite
      ]
  toEncoding NormalColors {..} =
    Aeson.pairs . mconcat $
      [ "black" Aeson..= normalBlack,
        "red" Aeson..= normalRed,
        "green" Aeson..= normalGreen,
        "yellow" Aeson..= normalYellow,
        "blue" Aeson..= normalBlue,
        "magenta" Aeson..= normalMagenta,
        "cyan" Aeson..= normalCyan,
        "white" Aeson..= normalWhite
      ]

data BrightColors
  = BrightColors
      { brightBlack :: Text,
        brightRed :: Text,
        brightGreen :: Text,
        brightYellow :: Text,
        brightBlue :: Text,
        brightMagenta :: Text,
        brightCyan :: Text,
        brightWhite :: Text
      }
  deriving (Show, Generic)

instance Aeson.ToJSON BrightColors where
  toJSON BrightColors {..} =
    Aeson.object
      [ "black" Aeson..= brightBlack,
        "red" Aeson..= brightRed,
        "green" Aeson..= brightGreen,
        "yellow" Aeson..= brightYellow,
        "blue" Aeson..= brightBlue,
        "magenta" Aeson..= brightMagenta,
        "cyan" Aeson..= brightCyan,
        "white" Aeson..= brightWhite
      ]
  toEncoding BrightColors {..} =
    Aeson.pairs . mconcat $
      [ "black" Aeson..= brightBlack,
        "red" Aeson..= brightRed,
        "green" Aeson..= brightGreen,
        "yellow" Aeson..= brightYellow,
        "blue" Aeson..= brightBlue,
        "magenta" Aeson..= brightMagenta,
        "cyan" Aeson..= brightCyan,
        "white" Aeson..= brightWhite
      ]

data Colors
  = Colors
      { primaryColors :: PrimaryColors,
        normalColors :: NormalColors,
        brightColors :: BrightColors
      }
  deriving (Show, Generic)

instance Aeson.ToJSON Colors where
  toJSON Colors {..} =
    Aeson.object
      [ "primary" Aeson..= primaryColors,
        "normal" Aeson..= normalColors,
        "bright" Aeson..= brightColors
      ]
