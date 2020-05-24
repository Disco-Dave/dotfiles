import           Data.Function                  ( (&) )

import           XMonad

import qualified XMonad.Util.SpawnOnce         as SpawnOnce

import qualified XMonad.Layout.NoBorders       as NoBordersLayout

import qualified XMonad.Hooks.DynamicLog       as DynamicLogHook
import qualified XMonad.Hooks.EwmhDesktops     as EwmhHook



-- Set variables for mod masks, since I can never remeber them.
superKey :: KeyMask
superKey = mod4Mask

altKey :: KeyMask
altKey = mod1Mask

myStartupHook = do
  SpawnOnce.spawnOnce "mpv $XDG_CONFIG_HOME/dotfiles/assets/startup.mp3"
  SpawnOnce.spawnOnce "xmobar $XDG_CONFIG_HOME/xmobar/xmobar.config"

myLayoutHook = NoBordersLayout.smartBorders $ layoutHook def

myConfig = def 
  { terminal = "alacritty"
  , modMask = mod1Mask 
  , startupHook = myStartupHook
  , layoutHook = myLayoutHook
  }

main :: IO ()
main = myConfig 
  & EwmhHook.ewmh
  & EwmhHook.ewmhFullscreen
  & DynamicLogHook.xmobar 
  >>= xmonad
