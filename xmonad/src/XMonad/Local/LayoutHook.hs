module XMonad.Local.LayoutHook where

import Data.Function ((&))
import qualified XMonad
import XMonad.Layout.Decoration (Decoration, DefaultShrinker)
import qualified XMonad.Layout.Decoration as Decoration
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.Master (AddMaster)
import qualified XMonad.Layout.Master as Master
import XMonad.Layout.Renamed (Rename)
import qualified XMonad.Layout.Renamed as Renamed
import XMonad.Layout.Simplest (Simplest)
import XMonad.Layout.Tabbed (TabbedDecoration)
import qualified XMonad.Layout.Tabbed as Tabbed
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

type Renamed = ModifiedLayout Rename

rename :: String -> l a -> ModifiedLayout Rename l a
rename newName =
  Renamed.renamed [Renamed.Replace newName]

type Tall = XMonad.Tall

tall :: Tall a
tall =
  let numOfDefMasters = 1
      ratio = 1 / 2
      delta = 3 / 300
   in XMonad.Tall numOfDefMasters ratio delta

type Wide = Renamed (XMonad.Mirror Tall)

wide :: Wide a
wide =
  rename "Wide" $ XMonad.Mirror tall

type TallTabbed =
  Renamed (ModifiedLayout AddMaster (ModifiedLayout (Decoration TabbedDecoration DefaultShrinker) Simplest))

tallTabbed ::
  ( Eq l
  , Decoration.LayoutModifier (Decoration TabbedDecoration DefaultShrinker) l
  ) =>
  Decoration.Theme ->
  TallTabbed l
tallTabbed theme =
  Tabbed.tabbed Tabbed.shrinkText theme
    & Master.mastered (1 / 100) (1 / 2)
    & rename "Tall Tabbed"
