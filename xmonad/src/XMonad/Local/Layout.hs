module XMonad.Local.Layout (
  LayoutName (..),
  layoutToString,
  layoutFromString,
  getLayoutName,
  layoutHook,
) where

import Control.Monad.Reader (Reader)
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
import qualified XMonad.StackSet as StackSet

data LayoutName
  = Tall
  | Wide
  | TallTabbed
  | Full
  | Other String
  deriving (Show, Eq, Ord)

layoutToString :: LayoutName -> String
layoutToString = \case
  Tall -> "Tall"
  Wide -> "Wide"
  TallTabbed -> "Tall Tabbed"
  Full -> "Full"
  Other unkownLayoutName -> unkownLayoutName

layoutFromString :: String -> LayoutName
layoutFromString = \case
  "Tall" -> Tall
  "Wide" -> Wide
  "Tall Tabbed" -> TallTabbed
  "Full" -> Full
  unkownLayoutName -> Other unkownLayoutName

getLayoutName :: X LayoutName
getLayoutName = do
  windowset <- XMonad.gets XMonad.windowset

  let workspace = StackSet.workspace $ StackSet.current windowset
      layoutString = XMonad.description $ StackSet.layout workspace
   in pure $ layoutFromString layoutString

decorationTheme :: Theme -> Decoration.Theme
decorationTheme theme =
  let windowColor color = Color.toHashString . color $ Theme.themeWindow theme
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

rename name =
  Renamed.renamed [Renamed.Replace $ layoutToString name]

tall =
  let numOfDefMasters = 1
      delta = 3 / 100
      ratio = 1 / 2
   in rename Tall $ XMonad.Tall numOfDefMasters delta ratio

wide =
  rename Wide $ XMonad.Mirror tall

tallTabbed theme =
  Tabbed.tabbed Tabbed.shrinkText theme
    & Master.mastered (3 / 100) (1 / 2)
    & rename TallTabbed

full =
  rename Full XMonad.Full

layoutHook = do
  theme <- Reader.asks @_ @(Reader Environment) envTheme
  pure $
    let hooks =
          (ManageDocks.avoidStruts @_ @XMonad.Window)
            . NoBorders.lessBorders NoBorders.Screen
            . Toggle.toggleLayouts full
        choices = tall ||| wide ||| tallTabbed (decorationTheme theme)
     in hooks choices
