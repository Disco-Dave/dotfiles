module XMonad.Local.Theme (
  WindowColors (..),
  XmobarColors (..),
  DmenuColors (..),
  Theme (..),
  nord,
) where

import XMonad.Local.Theme.Color (Color)
import XMonad.Local.Theme.Font (Font (..))
import qualified XMonad.Local.Theme.Palettes.Nord as Nord

data WindowColors = WindowColors
  { windowBorder :: Color
  , windowBorderFocussed :: Color
  , windowBorderUrgent :: Color
  , windowText :: Color
  , windowTextFocussed :: Color
  , windowTextUrgent :: Color
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
          { windowBorder = Nord.polarNight1
          , windowBorderFocussed = Nord.frost2
          , windowBorderUrgent = Nord.auroraRed
          , windowText = Nord.snowStorm2
          , windowTextFocussed = Nord.polarNight1
          , windowTextUrgent = Nord.snowStorm2
          }
    , themeXmobar =
        XmobarColors
          { xmobarBorder = Nord.polarNight1
          , xmobarBackground = Nord.polarNight1
          , xmobarForeground = Nord.snowStorm2
          , xmobarSelectedLayout = Nord.auroraYellow
          , xmobarTitle = Nord.auroraGreen
          }
    , themeDmenu =
        DmenuColors
          { dmenuNormalBackground = Nord.polarNight1
          , dmenuNormalForeground = Nord.snowStorm2
          , dmenuSelectedBackground = Nord.frost2
          , dmenuSelectedForeground = Nord.polarNight1
          }
    }
