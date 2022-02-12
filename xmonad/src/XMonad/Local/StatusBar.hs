module XMonad.Local.StatusBar (
  addStatusBar,
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT)
import qualified Control.Monad.Reader as Reader
import XMonad (X)
import qualified XMonad
import qualified XMonad.Hooks.StatusBar as StatusBar
import XMonad.Hooks.StatusBar.PP (PP)
import qualified XMonad.Hooks.StatusBar.PP as PP
import XMonad.Local.Environment (Environment (..))
import qualified XMonad.Local.Theme as Theme
import qualified XMonad.Local.Theme.Color as Color
import qualified XMonad.StackSet as StackSet

fullScreenWindowCount :: X (Maybe String)
fullScreenWindowCount = do
  workspace <- do
    windowset <- XMonad.gets XMonad.windowset
    pure . StackSet.workspace $ StackSet.current windowset

  let currentLayout = XMonad.description $ StackSet.layout workspace
      numberOfWindows = length . StackSet.integrate' $ StackSet.stack workspace

  pure $
    if currentLayout == "Full" && numberOfWindows > 0
      then Just $ show numberOfWindows
      else Nothing

pp :: ReaderT Environment X PP
pp = do
  theme <- Reader.asks envTheme

  let color xmobarColor =
        let hexColor = Color.toString . xmobarColor $ Theme.themeXmobar theme
         in PP.xmobarColor hexColor ""

  pure
    PP.def
      { PP.ppCurrent = color Theme.xmobarCurrentWs . PP.wrap "[" "]"
      , PP.ppVisible = color Theme.xmobarForeground . PP.wrap "(" ")"
      , PP.ppUrgent = color Theme.xmobarUrgentWs
      , PP.ppTitle = color Theme.xmobarWindowTitle
      , PP.ppTitleSanitize = PP.shorten 40
      , PP.ppExtras = [fullScreenWindowCount]
      }

addStatusBar ::
  XMonad.LayoutClass l XMonad.Window =>
  XMonad.XConfig l ->
  ReaderT Environment X (XMonad.XConfig l)
addStatusBar xconfig = do
  env <- Reader.ask
  sbConfig <- liftIO $ StatusBar.statusBarPipe "xmobar" (Reader.runReaderT pp env)
  pure $ StatusBar.withSB sbConfig xconfig
