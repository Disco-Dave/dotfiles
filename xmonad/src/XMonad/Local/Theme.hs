module XMonad.Local.Theme (
  Font (..),
  fontToXftText,
  updateFontSize,
  setFontSize,
  Color (..),
  colorToString,
  WindowColors (..),
  XmobarColors (..),
  DmenuColors (..),
  Theme (..),
  nord,
) where

import Data.Text (Text)
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified XMonad.Local.Colors.Nord as Nord

data Font = Font
  { fontFamily :: Text
  , fontSize :: Natural
  }
  deriving (Show, Eq)

fontToXftText :: Font -> Text
fontToXftText Font{..} =
  "xft:" <> fontFamily <> ":size=" <> Text.pack (show fontSize)

updateFontSize :: (Natural -> Natural) -> Font -> Font
updateFontSize updateSize font@Font{fontSize} =
  font{fontSize = updateSize fontSize}

setFontSize :: Natural -> Font -> Font
setFontSize fontSize =
  updateFontSize (const fontSize)

newtype Color = Color
  { colorToText :: Text
  }
  deriving (Show, Eq)

colorToString :: Color -> String
colorToString =
  Text.unpack . colorToText

data WindowColors = WindowColors
  { windowBorder :: Color
  , windowBorderFocussed :: Color
  }
  deriving (Show, Eq)

data XmobarColors = XmobarColors
  { xmobarBorder :: Color
  , xmobarBackground :: Color
  , xmobarForeground :: Color
  , xmobarSelectedLayout :: Color
  , xmobarTitle :: Color
  }
  deriving (Show, Eq)

data DmenuColors = DmenuColors
  { dmenuNormalBackground :: Color
  , dmenuNormalForeground :: Color
  , dmenuSelectedBackground :: Color
  , dmenuSelectedForeground :: Color
  }
  deriving (Show, Eq)

data Theme = Theme
  { themeFont :: Font
  , themeWindow :: WindowColors
  , themeXmobar :: XmobarColors
  , themeDmenu :: DmenuColors
  }
  deriving (Show, Eq)

nord :: Theme
nord =
  Theme
    { themeFont =
        Font
          { fontFamily = "FreeSans"
          , fontSize = 12
          }
    , themeWindow =
        WindowColors
          { windowBorder = Color Nord.polarNight1
          , windowBorderFocussed = Color Nord.frost2
          }
    , themeXmobar =
        XmobarColors
          { xmobarBorder = Color Nord.polarNight1
          , xmobarBackground = Color Nord.polarNight1
          , xmobarForeground = Color Nord.snowStorm2
          , xmobarSelectedLayout = Color Nord.auroraYellow
          , xmobarTitle = Color Nord.auroraGreen
          }
    , themeDmenu =
        DmenuColors
          { dmenuNormalBackground = Color Nord.polarNight1
          , dmenuNormalForeground = Color Nord.snowStorm2
          , dmenuSelectedBackground = Color Nord.frost2
          , dmenuSelectedForeground = Color Nord.polarNight1
          }
    }
