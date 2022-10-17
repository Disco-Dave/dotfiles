module XMonad.Local.StatusBar
  ( addStatusBar
  )
where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Reader qualified as Reader
import Control.Monad.Trans (lift)
import Shared.Environment (Environment (..))
import Shared.Hostname qualified as Hostname
import Shared.Theme qualified as Theme
import Shared.Theme.Color qualified as Color
import XMonad (X)
import XMonad qualified
import XMonad.Core (WorkspaceId)
import XMonad.Hooks.StatusBar qualified as StatusBar
import XMonad.Hooks.StatusBar.PP (PP, xmobarAction)
import XMonad.Hooks.StatusBar.PP qualified as PP
import XMonad.Hooks.StatusBar.WorkspaceScreen (combineWithScreenNumber)
import XMonad.Local.Layout (LayoutName (..), getLayoutName)
import XMonad.Local.Utils (numberOfWindowsOnWorkspace)
import XMonad.Util.WorkspaceCompare (getSortByTag)


fullScreenWindowCount :: X (Maybe String)
fullScreenWindowCount = do
  currentLayout <- getLayoutName
  numberOfWindows <- numberOfWindowsOnWorkspace
  pure $
    if currentLayout == Full && numberOfWindows > 0
      then Just $ show numberOfWindows
      else Nothing


makeClickable :: Monad m => String -> m String
makeClickable tag =
  pure $ xmobarAction ("xdotool key alt+" <> tag) "1" tag


prefixScreenName :: WorkspaceId -> String -> String
prefixScreenName workspaceId screenNumber =
  let screenName =
        case screenNumber of
          "0" -> " C"
          "2" -> " R"
          "1" -> " L"
          _ -> ""
   in workspaceId <> screenName


makePp :: ReaderT Environment X PP
makePp = do
  theme <- Reader.asks (.theme)

  let color xmobarColor =
        let hexColor = Color.toHashString $ xmobarColor theme.xmobar
         in PP.xmobarColor hexColor ""

  hostname <- Reader.asks (.hostname)

  let hostnameExtra =
        pure $ case hostname of
          Hostname.Work -> Just $ Hostname.toString Hostname.Work
          _ -> Nothing

  let addScreenName =
        case hostname of
          Hostname.Work -> combineWithScreenNumber prefixScreenName
          Hostname.Desktop -> combineWithScreenNumber prefixScreenName
          _ -> pure

  lift . addScreenName $
    PP.def
      { PP.ppCurrent = color (.currentWs) . PP.wrap "[" "]"
      , PP.ppVisible = color (.foreground) . PP.wrap "(" ")"
      , PP.ppUrgent = color (.urgentWs)
      , PP.ppTitle = color (.windowTitle)
      , PP.ppTitleSanitize = PP.xmobarRaw . PP.shorten 40
      , PP.ppExtras = [fullScreenWindowCount, hostnameExtra]
      , PP.ppRename = makeClickable
      , PP.ppSort = getSortByTag
      }


addStatusBar
  :: XMonad.LayoutClass l XMonad.Window
  => XMonad.XConfig l
  -> ReaderT Environment IO (XMonad.XConfig l)
addStatusBar xconfig = do
  env <- Reader.ask

  let pp = Reader.runReaderT makePp env
      primary = StatusBar.statusBarPropTo "_XMONAD_LOG_1" "xmobar" pp
      secondary = StatusBar.statusBarPropTo "_XMONAD_LOG_2" "xmobar --secondary" pp
      tertiary = StatusBar.statusBarPropTo "_XMONAD_LOG_3" "xmobar --tertiary" pp

  let sbConfig = mconcat $ case env.hostname of
        Hostname.Desktop -> [primary, secondary, tertiary]
        Hostname.Work -> [primary, secondary, tertiary]
        _ -> [primary]

  pure $ StatusBar.withSB sbConfig xconfig
