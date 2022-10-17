module Shared.Theme
  ( WindowColors (..)
  , XmobarColors (..)
  , DmenuColors (..)
  , Theme (..)
  , nord
  )
where

import Shared.Theme.Color (Color)
import Shared.Theme.Font (Font (..))
import qualified Shared.Theme.Palettes.Nord as Nord


data WindowColors = WindowColors
  { border :: Color
  , borderFocussed :: Color
  , borderUrgent :: Color
  , text :: Color
  , textFocussed :: Color
  , textUrgent :: Color
  }
  deriving (Show, Eq)


data XmobarColors = XmobarColors
  { border :: Color
  , background :: Color
  , foreground :: Color
  , selectedLayout :: Color
  , title :: Color
  , currentWs :: Color
  , urgentWs :: Color
  , windowTitle :: Color
  }
  deriving (Show, Eq)


data DmenuColors = DmenuColors
  { normalBackground :: Color
  , normalForeground :: Color
  , selectedBackground :: Color
  , selectedForeground :: Color
  }
  deriving (Show, Eq)


data Theme = Theme
  { font :: Font
  , window :: WindowColors
  , xmobar :: XmobarColors
  , dmenu :: DmenuColors
  }
  deriving (Show, Eq)


nord :: Theme
nord =
  Theme
    { font =
        Font
          { name = "FreeSans"
          , size = 12
          }
    , window =
        WindowColors
          { border = Nord.polarNight1
          , borderFocussed = Nord.frost2
          , borderUrgent = Nord.auroraRed
          , text = Nord.snowStorm2
          , textFocussed = Nord.polarNight1
          , textUrgent = Nord.snowStorm2
          }
    , xmobar =
        XmobarColors
          { border = Nord.polarNight1
          , background = Nord.polarNight1
          , foreground = Nord.snowStorm2
          , selectedLayout = Nord.auroraYellow
          , title = Nord.auroraGreen
          , currentWs = Nord.auroraYellow
          , windowTitle = Nord.auroraGreen
          , urgentWs = Nord.auroraRed
          }
    , dmenu =
        DmenuColors
          { normalBackground = Nord.polarNight1
          , normalForeground = Nord.snowStorm2
          , selectedBackground = Nord.frost2
          , selectedForeground = Nord.polarNight1
          }
    }
