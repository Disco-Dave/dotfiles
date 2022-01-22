module Main (main) where

import Control.Arrow ((>>>))
import Data.Foldable (for_)
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified MyKeys
import qualified MyXmobar
import XMonad
import XMonad.Hooks.DynamicLog (xmobarAction)
import qualified XMonad.Hooks.DynamicLog as DynamicLog
import qualified XMonad.Hooks.EwmhDesktops as Ewmh
import qualified XMonad.Hooks.ManageDocks as ManageDocks
import qualified XMonad.Hooks.ManageHelpers as ManageHelpers
import XMonad.Hooks.SetWMName (setWMName)
import qualified XMonad.Layout.Master as Master
import qualified XMonad.Layout.Named as Named
import qualified XMonad.Layout.NoBorders as NoBorders
import qualified XMonad.Layout.Tabbed as Tabbed
import qualified XMonad.Layout.ToggleLayouts as Toggle
import qualified XMonad.StackSet as StackSet
import qualified XMonad.Util.SpawnOnce as SpawnOnce

myStartupHook isDesktop = do
  setWMName "LG3D"
  screenWorkspace 0 >>= flip whenJust (windows . StackSet.view)
  for_ autoStartCommands SpawnOnce.spawnOnce
 where
  machineSpecifcCommands =
    if not isDesktop
      then ["stalonetray -c $XDG_CONFIG_HOME/stalonetray/stalonetrayrc"]
      else
        [ "stalonetray -c $XDG_CONFIG_HOME/stalonetray/stalonetrayrc --icon-size 22 --slot-size 30 --geometry \"1x1-3840+0\" --max-geometry \"50x1-3840+0\""
        , "birdtray -H"
        ]
  autoStartCommands =
    [ "mpv $XDG_CONFIG_HOME/dotfiles/assets/startup.mp3"
    , "nm-applet"
    , "xfce4-power-manager"
    , "pasystray"
    , "blueman-applet"
    , "/usr/lib/xfce4/notifyd/xfce4-notifyd"
    , "lxsession"
    ]
      <> machineSpecifcCommands

myLayoutHook = hooks layout
 where
  hooks =
    Toggle.toggleLayouts Full
      >>> NoBorders.lessBorders NoBorders.Screen
      >>> ManageDocks.avoidStruts
  layout = tiled ||| Mirror tiled ||| masterAndTabs
   where
    -- default tiling algorithm partitions the screen into two panes
    tiled = Tall nmaster delta ratio
    -- The default number of windows in the master pane
    nmaster = 1
    -- Default proportion of screen occupied by master pane
    ratio = 1 / 2
    -- Percent of screen to increment by when resizing panes
    delta = 3 / 100
    masterAndTabs =
      Tabbed.tabbed Tabbed.shrinkText theme
        & Master.mastered (1 / 100) (1 / 2)
        & Named.named "Master Tabbed"
     where
      theme =
        def
          { Tabbed.fontName = "xft:FreeSans:size=11"
          , Tabbed.activeColor = "#81A1C1"
          , Tabbed.activeBorderColor = "#81A1C1"
          , Tabbed.activeTextColor = "#3B4252"
          , Tabbed.activeBorderWidth = 0
          , Tabbed.inactiveColor = "#3B4252"
          , Tabbed.inactiveBorderColor = "#3B4252"
          , Tabbed.inactiveTextColor = "#ECEFF4"
          , Tabbed.inactiveBorderWidth = 0
          , Tabbed.urgentColor = "#BF616A"
          , Tabbed.urgentBorderColor = "#BF616A"
          , Tabbed.urgentBorderWidth = 0
          }

myLogHook logOutput = DynamicLog.dynamicLogWithPP myXmobarPp
 where
  myXmobarPp =
    def
      { DynamicLog.ppCurrent = xmobarColor "#EBCB8B" "" . wrap "[" "]"
      , DynamicLog.ppTitle = xmobarColor "#A3BE8C" "" . shorten 40
      , DynamicLog.ppVisible = wrap "(" ")"
      , DynamicLog.ppUrgent = xmobarColor "#BF616A" "#EBCB8B"
      , DynamicLog.ppOutput = logOutput
      , DynamicLog.ppExtras = [windowCount]
      , DynamicLog.ppLayout = xmobarAction "1" "xdotool key alt+space"
      }
   where
    windowCount = do
      windowset' <- gets windowset
      let workspace' = windowset' & StackSet.current & StackSet.workspace
      let numWindows =
            workspace' & StackSet.stack & StackSet.integrate' & length
      let layout = workspace' & StackSet.layout & description
      pure $
        if numWindows > 0 && layout == "Full"
          then Just $ xmobarColor "#A3BE8C" "" (show numWindows)
          else Nothing
    wrap _ _ "" = ""
    wrap l r m = l <> m <> r
    xmobarColor fg bg = wrap t "</fc>"
     where
      t = concat ["<fc=", fg, if null bg then "" else "," <> bg, ">"]
    shorten n xs
      | length xs < n = xs
      | otherwise = take (n - length end) xs <> end
     where
      end = "..."

myManageHook =
  mconcat -- use xprop to find this information
    [ className =? "Pavucontrol" --> ManageHelpers.doCenterFloat
    ]

myWorkspaces = do
  num <- fmap show ([1 .. 9] :: [Integer])
  pure $ xmobarAction "1" ("xdotool key alt+" <> num) num

makeConfig isDesktop handles = hooks config'
 where
  hooks = Ewmh.ewmh >>> Ewmh.ewmhFullscreen >>> ManageDocks.docks
  config' =
    def
      { terminal = "alacritty"
      , focusedBorderColor = "#81A1C1"
      , normalBorderColor = "#3B4252"
      , borderWidth = 3
      , modMask = MyKeys.altKey
      , keys = MyKeys.keys
      , layoutHook = myLayoutHook
      , logHook = myLogHook handles
      , manageHook = myManageHook
      , startupHook = myStartupHook isDesktop
      , workspaces = myWorkspaces
      }

main :: IO ()
main = do
  isDesktop <- Text.readFile "/etc/hostname" <&> Text.strip <&> (== "desktop")

  let xmobarSettings =
        if isDesktop
          then [MyXmobar.left]
          else [MyXmobar.left, MyXmobar.right]

  MyXmobar.withXmobar xmobarSettings $
    xmonad . makeConfig isDesktop
