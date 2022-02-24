module XMonad.Local.StartupHook (
  startupHook,
) where

import Control.Monad.List (lift)
import Control.Monad.Reader (ReaderT)
import qualified Control.Monad.Reader as Reader
import Data.Foldable (traverse_)
import qualified System.FilePath as FilePath
import XMonad (X)
import qualified XMonad
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Local.Environment (Environment (..))
import XMonad.Local.FilePaths as FilePaths
import qualified XMonad.Local.Hostname as Hostname
import qualified XMonad.Local.Theme as Theme
import XMonad.Local.Theme.Color (Color)
import qualified XMonad.Local.Theme.Color as Color
import qualified XMonad.StackSet as StackSet
import XMonad.Util.SpawnOnce (spawnOnce)

trayer :: Color -> [String] -> String
trayer color extraFlags =
  let defaultFlags =
        [ "--monitor primary"
        , "--widthtype request"
        , "--edge top"
        , "--align right"
        , "--alpha 0"
        , "--transparent true"
        , "--tint " <> Color.to0xString color
        ]
      flags = unwords $ defaultFlags <> extraFlags
   in "trayer " <> flags

focusPrimaryScreen :: X ()
focusPrimaryScreen = do
  primaryScreenId <- XMonad.screenWorkspace 0
  case primaryScreenId of
    Nothing -> pure ()
    Just screenId ->
      XMonad.windows $ StackSet.view screenId

startupHook :: ReaderT Environment X ()
startupHook = do
  lift $ setWMName "LG3D" -- Fixes GUIs built with Java
  lift focusPrimaryScreen

  configHome <- Reader.asks (FilePaths.xdgConfig . envFilePaths)
  hostname <- Reader.asks envHostname

  Theme.XmobarColors{xmobarBackground} <- Reader.asks (Theme.themeXmobar . envTheme)

  let startupSound =
        FilePath.joinPath
          [ configHome
          , "dotfiles"
          , "assets"
          , "startup.mp3"
          ]
      extraTrayerFlags =
        case hostname of
          Hostname.Desktop -> [" --height 30", "--iconspacing 8"]
          Hostname.Work -> [" --height 30", "--iconspacing 8"]
          _ -> ["--height 22", "--iconspacing 5"]


  lift . traverse_ spawnOnce $
    [ "mpv " <> startupSound
    , trayer xmobarBackground extraTrayerFlags
    , "lxsession"
    , "nm-applet"
    , "/usr/bin/xfce4-power-manager"
    , "/usr/lib/xfce4/notifyd/xfce4-notifyd"
    , "pasystray"
    , "blueman-applet"
    , "birdtray -H"
    , "redshift-gtk -l 40.19342:-76.7633"
    ]
