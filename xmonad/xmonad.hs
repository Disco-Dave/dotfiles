import           Data.Function                  ( (&) )
import           Data.Foldable                  ( for_ )

import           XMonad

import qualified XMonad.Util.Run               as Run
import qualified XMonad.Util.SpawnOnce         as SpawnOnce

import qualified XMonad.Layout.NoBorders       as NoBordersLayout

import qualified XMonad.Hooks.DynamicLog       as DynamicLogHook
import qualified XMonad.Hooks.EwmhDesktops     as EwmhHook
import qualified XMonad.Hooks.ManageDocks      as ManageDocksHook


(superKey, altKey) = (mod4Mask, mod1Mask)

myStartupHook = do
  SpawnOnce.spawnOnce "mpv $XDG_CONFIG_HOME/dotfiles/assets/startup.mp3"
  SpawnOnce.spawnOnce "nm-applet"
  SpawnOnce.spawnOnce "xfce4-power-manager"
  SpawnOnce.spawnOnce "pasystray"
  SpawnOnce.spawnOnce "redshift-gtk"
  SpawnOnce.spawnOnce "/usr/lib/xfce4/notifyd/xfce4-notifyd"

myLayoutHook = layoutHook def
  & NoBordersLayout.smartBorders
  & ManageDocksHook.avoidStruts

makeConfig handles = def
    { terminal    = "alacritty"
    , modMask     = mod1Mask
    , startupHook = myStartupHook
    , layoutHook  = myLayoutHook
    , logHook     = DynamicLogHook.dynamicLogWithPP $ DynamicLogHook.xmobarPP
      { DynamicLogHook.ppOutput = \p -> for_ handles $ \h -> Run.hPutStrLn h p
      }
    }
  & EwmhHook.ewmh
  & EwmhHook.ewmhFullscreen
  & ManageDocksHook.docks

main :: IO ()
main = do
  xmobar <- Run.spawnPipe "xmobar-primary"
  _ <- Run.spawnPipe "stalonetray -c $XDG_CONFIG_HOME/stalonetray/stalonetrayrc"
  let config = makeConfig [xmobar]
  xmonad config
