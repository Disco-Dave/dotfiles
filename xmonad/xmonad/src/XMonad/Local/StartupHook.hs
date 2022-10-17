module XMonad.Local.StartupHook
  ( startupHook
  )
where

import Control.Monad.List (lift)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Reader qualified as Reader
import Data.Foldable (traverse_)
import Shared.Environment (Environment (..))
import Shared.FilePaths as FilePaths
import Shared.Hostname qualified as Hostname
import Shared.Theme qualified as Theme
import Shared.Theme.Color (Color)
import Shared.Theme.Color qualified as Color
import System.FilePath qualified as FilePath
import XMonad (X)
import XMonad qualified
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.StackSet qualified as StackSet
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

  configHome <- Reader.asks (.filePaths.xdgConfig)
  hostname <- Reader.asks (.hostname)

  xmobar <- Reader.asks (.theme.xmobar)

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
          Hostname.Work -> [" --height 30", "--iconspacing 5"]
          _ -> ["--height 22", "--iconspacing 5"]

  lift . traverse_ spawnOnce $
    [ "mpv " <> startupSound
    , trayer xmobar.background extraTrayerFlags
    , "lxsession"
    , "nm-applet"
    , "/usr/bin/xfce4-power-manager"
    , "/usr/lib/xfce4/notifyd/xfce4-notifyd"
    , "pasystray"
    , "blueman-applet"
    , "birdtray -H"
    , "redshift-gtk -l 40.19342:-76.7633"
    , "update-firefox-user-js.sh"
    ]
