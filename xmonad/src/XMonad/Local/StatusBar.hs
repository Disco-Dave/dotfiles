module XMonad.Local.StatusBar (
  addStatusBar,
) where

import Control.Monad.Reader (ReaderT)
import qualified Control.Monad.Reader as Reader
import Control.Monad.Trans (lift)
import XMonad (X)
import qualified XMonad
import qualified XMonad.Hooks.StatusBar as StatusBar
import XMonad.Hooks.StatusBar.PP (PP)
import qualified XMonad.Hooks.StatusBar.PP as PP
import XMonad.Local.Environment (Environment (..))
import qualified XMonad.Local.Hostname as Hostname
import XMonad.Local.Layout (LayoutName (..), getLayoutName)
import qualified XMonad.Local.Theme as Theme
import qualified XMonad.Local.Theme.Color as Color
import XMonad.Local.Utils (numberOfWindowsOnWorkspace)
import XMonad.Util.ClickableWorkspaces (clickablePP)

fullScreenWindowCount :: X (Maybe String)
fullScreenWindowCount = do
  currentLayout <- getLayoutName
  numberOfWindows <- numberOfWindowsOnWorkspace
  pure $
    if currentLayout == Full && numberOfWindows > 0
      then Just $ show numberOfWindows
      else Nothing

makePp :: ReaderT Environment X PP
makePp = do
  theme <- Reader.asks envTheme

  let color xmobarColor =
        let hexColor = Color.toHashString . xmobarColor $ Theme.themeXmobar theme
         in PP.xmobarColor hexColor ""

  lift . clickablePP $
    PP.def
      { PP.ppCurrent = color Theme.xmobarCurrentWs . PP.wrap "[" "]"
      , PP.ppVisible = color Theme.xmobarForeground . PP.wrap "(" ")"
      , PP.ppUrgent = color Theme.xmobarUrgentWs
      , PP.ppTitle = color Theme.xmobarWindowTitle
      , PP.ppTitleSanitize = PP.xmobarRaw . PP.shorten 40
      , PP.ppExtras = [fullScreenWindowCount]
      }

addStatusBar ::
  XMonad.LayoutClass l XMonad.Window =>
  XMonad.XConfig l ->
  ReaderT Environment IO (XMonad.XConfig l)
addStatusBar xconfig = do
  env <- Reader.ask

  let pp = Reader.runReaderT makePp env
      primary = StatusBar.statusBarPropTo "_XMONAD_LOG_1" "xmobar" pp
      secondary = StatusBar.statusBarPropTo "_XMONAD_LOG_2" "xmobar --secondary" pp

  let sbConfig = mconcat $ case envHostname env of
        Hostname.Desktop -> [primary, secondary]
        _ -> [primary]

  pure $ StatusBar.withSB sbConfig xconfig
