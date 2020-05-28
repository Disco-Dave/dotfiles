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

-- TODO: Change colors for dmenu
-- TODO: Change colors for XMobar
-- TODO: Investigate how sub layouts are used

(superKey, altKey) = (mod4Mask, mod1Mask)


additionalKeys XConfig{..} = 
  [ -- Volume keys
    ((X11.noModMask, X11.xF86XK_AudioRaiseVolume), spawn "amixer -D pulse sset Master 5%+")
  , ((X11.noModMask, X11.xF86XK_AudioLowerVolume), spawn "amixer -D pulse sset Master 5%-")
  , ((X11.shiftMask, X11.xF86XK_AudioRaiseVolume), spawn "amixer -D pulse sset Master 1%+")
  , ((X11.shiftMask, X11.xF86XK_AudioLowerVolume), spawn "amixer -D pulse sset Master 1%-")
  , ((X11.noModMask, X11.xF86XK_AudioMute), spawn "amixer -D pulse sset Master toggle")
  , ((X11.noModMask, X11.xF86XK_AudioMicMute), spawn "pactl set-source-mute 1 toggle")
  , ((superKey, xK_v), spawn "pavucontrol")

  -- Web browser
  , ((superKey, xK_w), spawn "firefox")

  -- Screen shooter
  , ((X11.noModMask, xK_Print), spawn "xfce4-screenshooter")
  , ((superKey, xK_Print), spawn "xfce4-screenshooter -w")
  , ((X11.controlMask, xK_Print), spawn "xfce4-screenshooter -r")
  , ((X11.shiftMask, xK_Print), spawn "xfce4-screenshooter -f")
  
  , ((superKey, xK_Escape), kill)
  , ((modMask, xK_p), spawn "dmenu_run -fn \"FreeSans-12\"")
  ]


myStartupHook = 
  let autoStartCommands = 
        [ "mpv $XDG_CONFIG_HOME/dotfiles/assets/startup.mp3"
        , "stalonetray -c $XDG_CONFIG_HOME/stalonetray/stalonetrayrc"
        , "nm-applet"
        , "xfce4-power-manager"
        , "pasystray"
        , "redshift-gtk"
        , "blueman-applet"
        , "/usr/lib/xfce4/notifyd/xfce4-notifyd"
        ]
  in for_ autoStartCommands SpawnOnce.spawnOnce


myLayoutHook = layoutHook def
  & NoBordersLayout.smartBorders
  & ManageDocksHook.avoidStruts


makeConfig handles = def
  { terminal    = "alacritty"
  , focusedBorderColor = "#81A1C1"
  , normalBorderColor = "#3B4252"
  , borderWidth = 3
  , modMask     = mod1Mask
  , startupHook = myStartupHook
  , layoutHook  = myLayoutHook
  , logHook     = DynamicLogHook.dynamicLogWithPP $ DynamicLogHook.xmobarPP
    { DynamicLogHook.ppOutput = for_ handles . flip Run.hPutStrLn
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
