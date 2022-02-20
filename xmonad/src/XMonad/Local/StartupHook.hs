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
import qualified XMonad.Local.Theme.Color as Color
import qualified XMonad.StackSet as StackSet
import XMonad.Util.SpawnOnce (spawnOnce)

startupHook :: ReaderT Environment X ()
startupHook = do
  lift $ setWMName "LG3D"

  primaryScreenId <- lift $ XMonad.screenWorkspace 0

  case primaryScreenId of
    Nothing ->
      pure ()
    Just screenId ->
      lift . XMonad.windows $
        StackSet.view screenId

  configHome <- Reader.asks (FilePaths.xdgConfig . envFilePaths)
  hostname <- Reader.asks (Hostname.toString . envHostname)

  Theme.XmobarColors{xmobarBackground} <- Reader.asks (Theme.themeXmobar . envTheme)

  let startupSound =
        FilePath.joinPath
          [ configHome
          , "dotfiles"
          , "assets"
          , "startup.mp3"
          ]
      trayer =
        let trayerTint = Color.toString0x xmobarBackground
            baseCommand = "trayer --monitor primary --widthtype request --edge top --align right --alpha 0 --transparent true --iconspacing 10 --tint" <> trayerTint
         in case hostname of
              "Desktop" -> baseCommand <> " --height 30"
              _ -> baseCommand <> " --height 22"

  lift . traverse_ spawnOnce $
    [ "mpv " <> startupSound
    , trayer
    , "nm-applet"
    , "/usr/bin/xfce4-power-manager"
    , "pasystray"
    , "blueman-applet"
    , "lxsession"
    , "birdtray -H"
    , "redshift-gtk -l 40.19342:-76.7633"
    , "/usr/lib/xfce4/notifyd/xfce4-notifyd"
    ]
