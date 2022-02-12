module XMonad.Local.LayoutHook (
  layoutHook,
) where

import Control.Monad.Reader (ReaderT)
import qualified Control.Monad.Reader as Reader
import Data.Function ((&))
import XMonad (X, (|||))
import qualified XMonad
import qualified XMonad.Hooks.ManageDocks as ManageDocks
import qualified XMonad.Layout.Decoration as Decoration
import qualified XMonad.Layout.Master as Master
import qualified XMonad.Layout.NoBorders as NoBorders
import qualified XMonad.Layout.Renamed as Renamed
import qualified XMonad.Layout.Tabbed as Tabbed
import qualified XMonad.Layout.ToggleLayouts as Toggle
import XMonad.Local.Environment (Environment (envTheme))
import XMonad.Local.Theme (Theme)
import qualified XMonad.Local.Theme as Theme
import qualified XMonad.Local.Theme.Color as Color
import qualified XMonad.Local.Theme.Font as Font

decorationTheme :: Theme -> Decoration.Theme
decorationTheme theme =
  let windowColor color = Color.toString . color $ Theme.themeWindow theme
   in Decoration.def
        { Decoration.fontName = Font.toXftString $ Theme.themeFont theme
        , Decoration.activeBorderColor = windowColor Theme.windowBorderFocussed
        , Decoration.activeColor = windowColor Theme.windowBorderFocussed
        , Decoration.activeTextColor = windowColor Theme.windowTextFocussed
        , Decoration.inactiveBorderColor = windowColor Theme.windowBorder
        , Decoration.inactiveColor = windowColor Theme.windowBorder
        , Decoration.inactiveTextColor = windowColor Theme.windowText
        , Decoration.urgentBorderColor = windowColor Theme.windowBorderUrgent
        , Decoration.urgentColor = windowColor Theme.windowBorderUrgent
        , Decoration.urgentTextColor = windowColor Theme.windowTextUrgent
        }

rename newName =
  Renamed.renamed [Renamed.Replace newName]

tall =
  let numOfDefMasters = 1
      ratio = 1 / 2
      delta = 3 / 300
   in XMonad.Tall numOfDefMasters ratio delta

wide =
  rename "Wide" $ XMonad.Mirror tall

tallTabbed theme =
  Tabbed.tabbed Tabbed.shrinkText theme
    & Master.mastered (1 / 100) (1 / 2)
    & rename "Tall Tabbed"

layoutHook = do
  theme <- Reader.asks @_ @(ReaderT Environment X) envTheme
  pure $
    let hooks =
          (ManageDocks.avoidStruts @_ @XMonad.Window)
            . NoBorders.lessBorders NoBorders.Screen
            . Toggle.toggleLayouts XMonad.Full
        choices = tall ||| wide ||| tallTabbed (decorationTheme theme)
     in hooks choices
