{-# LANGUAGE RecordWildCards #-}

import           Data.Foldable                  ( for_ )
import           Data.Function                  ( (&) )

import           XMonad

import qualified XMonad.Util.EZConfig          as EZConfig
import qualified XMonad.Util.Run               as Run
import qualified XMonad.Util.SpawnOnce         as SpawnOnce

import qualified XMonad.Layout.NoBorders       as NoBordersLayout

import qualified XMonad.Hooks.DynamicLog       as DynamicLogHook
import qualified XMonad.Hooks.EwmhDesktops     as EwmhHook
import qualified XMonad.Hooks.ManageDocks      as ManageDocksHook

import qualified Graphics.X11                  as X11
import qualified Graphics.X11.ExtraTypes       as X11


(superKey, altKey) = (mod4Mask, mod1Mask)


additionalKeys XConfig{..} = 
  [ -- Volume keys
    ((X11.noModMask, X11.xF86XK_AudioRaiseVolume), spawn "amixer -D pulse sset Master 5%+")
  , ((X11.noModMask, X11.xF86XK_AudioLowerVolume), spawn "amixer -D pulse sset Master 5%-")
  , ((X11.shiftMask, X11.xF86XK_AudioRaiseVolume), spawn "amixer -D pulse sset Master 1%+")
  , ((X11.shiftMask, X11.xF86XK_AudioLowerVolume), spawn "amixer -D pulse sset Master 1%-")
  , ((X11.noModMask, X11.xF86XK_AudioMute), spawn "amixer -D pulse sset Master toggle")
  , ((superKey, xK_v), spawn "pavucontrol")

  -- Web browser
  , ((superKey, xK_w), spawn "firefox")

  -- Screen shooter
  , ((X11.noModMask, xK_Print), spawn "xfce4-screenshooter")
  , ((X11.controlMask, xK_Print), spawn "xfce4-screenshooter -r")
  , ((X11.shiftMask, xK_Print), spawn "xfce4-screenshooter -f")
  ]


myStartupHook = do
  SpawnOnce.spawnOnce "mpv $XDG_CONFIG_HOME/dotfiles/assets/startup.mp3"
  SpawnOnce.spawnOnce "stalonetray -c $XDG_CONFIG_HOME/stalonetray/stalonetrayrc"
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
  , focusedBorderColor = "#0066ff"
  , borderWidth = 2
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
    & (\c -> c `EZConfig.additionalKeys` additionalKeys c)


main :: IO ()
main = do
  xmobar <- Run.spawnPipe "xmobar-primary"
  let config = makeConfig [xmobar]
  xmonad config
