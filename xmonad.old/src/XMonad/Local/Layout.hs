module XMonad.Local.Layout (
  LayoutName (..),
  layoutToString,
  layoutFromString,
  getLayoutName,
  layoutHook,
) where

import Control.Monad.Reader (Reader)
import qualified Control.Monad.Reader as Reader
import qualified Data.List.NonEmpty as NonEmpty
import XMonad (X, (|||))
import qualified XMonad
import qualified XMonad.Hooks.ManageDocks as ManageDocks
import XMonad.Layout.BinarySpacePartition (emptyBSP)
import qualified XMonad.Layout.Decoration as Decoration
import XMonad.Layout.MultiToggle (mkToggle, single)
import qualified XMonad.Layout.NoBorders as NoBorders
import XMonad.Layout.Reflect (REFLECTX (REFLECTX), REFLECTY (REFLECTY))
import qualified XMonad.Layout.Renamed as Renamed
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
  | Full
  | Bsp
  | Other
  deriving (Show, Eq, Ord, Enum, Bounded)

layoutToString :: LayoutName -> String
layoutToString = \case
  Tall -> "Tall"
  Wide -> "Wide"
  Full -> "Full"
  Bsp -> "BSP"
  Other -> "Other"

layoutFromString :: String -> LayoutName
layoutFromString rawLayoutName =
  case fmap NonEmpty.last . NonEmpty.nonEmpty $ words rawLayoutName of
    Just "Tall" -> Tall
    Just "Wide" -> Wide
    Just "Full" -> Full
    Just "BSP" -> Bsp
    _ -> Other

getLayoutName :: X LayoutName
getLayoutName = do
  windowset <- XMonad.gets XMonad.windowset

  let workspace = StackSet.workspace $ StackSet.current windowset
      layoutString = XMonad.description $ StackSet.layout workspace
   in pure $ layoutFromString layoutString

_decorationTheme :: Theme -> Decoration.Theme
_decorationTheme theme =
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

full =
  rename Full XMonad.Full

bsd =
  rename Bsp emptyBSP

layoutHook = do
  _theme <- Reader.asks @_ @(Reader Environment) envTheme
  pure $
    let hooks =
          (ManageDocks.avoidStruts @_ @XMonad.Window)
            . NoBorders.lessBorders NoBorders.Screen
            . Toggle.toggleLayouts full
            . mkToggle (single REFLECTX)
            . mkToggle (single REFLECTY)
        choices = tall ||| wide ||| bsd
     in hooks choices
