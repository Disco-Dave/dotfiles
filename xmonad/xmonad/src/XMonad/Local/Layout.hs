{-# OPTIONS_GHC -Wno-missing-signatures #-}

module XMonad.Local.Layout
  ( LayoutName (..)
  , layoutToText
  , layoutToString
  , layoutFromText
  , layoutFromString
  , getLayoutName
  , layoutHook
  )
where

import Control.Monad.Reader (Reader)
import Control.Monad.Reader qualified as Reader
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Shared.Environment (Environment)
import Shared.Environment qualified as Environment
import Shared.Theme (Theme)
import Shared.Theme qualified as Theme
import Shared.Theme.Color qualified as Color
import Shared.Theme.Font qualified as Font
import Shared.Utils (enumFromText)
import XMonad (X, (|||))
import XMonad qualified
import XMonad.Hooks.ManageDocks qualified as ManageDocks
import XMonad.Layout.BinarySpacePartition (emptyBSP)
import XMonad.Layout.Decoration qualified as Decoration
import XMonad.Layout.MultiToggle (mkToggle, single)
import XMonad.Layout.NoBorders qualified as NoBorders
import XMonad.Layout.Reflect (REFLECTX (REFLECTX), REFLECTY (REFLECTY))
import XMonad.Layout.Renamed qualified as Renamed
import XMonad.Layout.ToggleLayouts qualified as Toggle
import XMonad.StackSet qualified as StackSet


data LayoutName
  = Tall
  | Wide
  | Full
  | Bsp
  | Other
  deriving (Show, Eq, Ord, Enum, Bounded)


layoutToText :: LayoutName -> Text
layoutToText = \case
  Tall -> "Tall"
  Wide -> "Wide"
  Full -> "Full"
  Bsp -> "BSP"
  Other -> "Other"


layoutToString :: LayoutName -> String
layoutToString =
  Text.unpack . layoutToText


layoutFromText :: Text -> LayoutName
layoutFromText rawLayoutName =
  let layoutWords = Text.words rawLayoutName

      lastLayoutWord =
        maybe "" NonEmpty.last (NonEmpty.nonEmpty layoutWords)
   in fromMaybe Other $ enumFromText layoutToText lastLayoutWord


layoutFromString :: String -> LayoutName
layoutFromString =
  layoutFromText . Text.pack


getLayoutName :: X LayoutName
getLayoutName = do
  windowset <- XMonad.gets XMonad.windowset

  let workspace = StackSet.workspace $ StackSet.current windowset
      layoutString = XMonad.description $ StackSet.layout workspace
   in pure $ layoutFromString layoutString


_decorationTheme :: Theme -> Decoration.Theme
_decorationTheme theme =
  let windowColor color = Color.toHashString . color $ theme.window
   in Decoration.def
        { Decoration.fontName = Font.toXftString $ theme.font
        , Decoration.activeBorderColor = windowColor (.borderFocussed)
        , Decoration.activeColor = windowColor (.borderFocussed)
        , Decoration.activeTextColor = windowColor (.textFocussed)
        , Decoration.inactiveBorderColor = windowColor (.border)
        , Decoration.inactiveColor = windowColor (.border)
        , Decoration.inactiveTextColor = windowColor (.text)
        , Decoration.urgentBorderColor = windowColor (.borderUrgent)
        , Decoration.urgentColor = windowColor (.borderUrgent)
        , Decoration.urgentTextColor = windowColor (.textUrgent)
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
  _theme <- Reader.asks @_ @(Reader Environment) (.theme)
  pure $
    let hooks =
          (ManageDocks.avoidStruts @_ @XMonad.Window)
            . NoBorders.lessBorders NoBorders.Screen
            . Toggle.toggleLayouts full
            . mkToggle (single REFLECTX)
            . mkToggle (single REFLECTY)
        choices = tall ||| wide ||| bsd
     in hooks choices
