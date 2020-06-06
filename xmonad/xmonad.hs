{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow ((>>>))
import Data.Foldable (for_)
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Graphics.X11.ExtraTypes as X11
import System.Exit (exitSuccess)
import XMonad
import qualified XMonad.Actions.SwapWorkspaces as Swap
import qualified XMonad.Hooks.DynamicLog as DynamicLog
import qualified XMonad.Hooks.EwmhDesktops as Ewmh
import qualified XMonad.Hooks.ManageDocks as ManageDocks
import qualified XMonad.Hooks.ManageHelpers as ManageHelpers
import qualified XMonad.Layout.Master as Master
import qualified XMonad.Layout.Named as Named
import qualified XMonad.Layout.NoBorders as NoBorders
import qualified XMonad.Layout.Tabbed as Tabbed
import qualified XMonad.Layout.ToggleLayouts as Toggle
import qualified XMonad.StackSet as StackSet
import qualified XMonad.Util.Run as Run
import qualified XMonad.Util.SpawnOnce as SpawnOnce

superKey = mod4Mask

altKey = mod1Mask

myKeys conf@XConfig {XMonad.modMask = modMask} =
  Map.fromList $
    -- launching and killing programs
    [ ((modMask, xK_Return), spawn $ XMonad.terminal conf), -- %! Launch terminal
      ((modMask .|. shiftMask, xK_c), kill), -- %! Close the focused window
      ((superKey, xK_Escape), kill),
      ((modMask, xK_p), spawn "dmenu_run -fn \"FreeSans-12\" -nb \"#3B4252\" -sb \"#81A1C1\" -nf \"#ECEFF4\" -sf \"#3B4252\""),
      ((superKey, xK_w), spawn "firefox"),
      -- changing layout commands
      ((modMask, xK_space), sendMessage NextLayout), -- %! Rotate through the available layout algorithms
      ((modMask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf), -- %!  Reset the layouts on the current workspace to default
      ((modMask, xK_f), sendMessage Toggle.ToggleLayout), -- %! Toggle the Full layout
      ((modMask, xK_n), refresh), -- %! Resize viewed windows to the correct size
      ((modMask, xK_s), sendMessage ManageDocks.ToggleStruts),
      -- move focus up or down the window stack
      ((modMask, xK_Tab), windows StackSet.focusDown), -- %! Move focus to the next window
      ((modMask .|. shiftMask, xK_Tab), windows StackSet.focusUp), -- %! Move focus to the previous window
      ((modMask, xK_j), windows StackSet.focusDown), -- %! Move focus to the next window
      ((modMask, xK_k), windows StackSet.focusUp), -- %! Move focus to the previous window
      ((modMask, xK_m), windows StackSet.focusMaster), -- %! Move focus to the master window

      -- modifying the window order
      ((modMask .|. shiftMask, xK_Return), windows StackSet.swapMaster), -- %! Swap the focused window and the master window
      ((modMask .|. shiftMask, xK_j), windows StackSet.swapDown), -- %! Swap the focused window with the next window
      ((modMask .|. shiftMask, xK_k), windows StackSet.swapUp), -- %! Swap the focused window with the previous window

      -- resizing the master/slave ratio
      ((modMask, xK_h), sendMessage Shrink), -- %! Shrink the master area
      ((modMask, xK_l), sendMessage Expand), -- %! Expand the master area

      -- floating layer support
      ((modMask, xK_t), withFocused $ windows . StackSet.sink), -- %! Push window back into tiling

      -- increase or decrease number of windows in the master area
      ((modMask, xK_comma), sendMessage (IncMasterN 1)), -- %! Increment the number of windows in the master area
      ((modMask, xK_period), sendMessage (IncMasterN (-1))), -- %! Deincrement the number of windows in the master area

      -- quit, or restart
      ((modMask .|. shiftMask, xK_q), io exitSuccess), -- %! Quit xmonad
      ((modMask, xK_q), spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"), -- %! Restart xmonad

      -- Volume keys
      ((noModMask, X11.xF86XK_AudioRaiseVolume), spawn "amixer -D pulse sset Master 5%+"),
      ((noModMask, X11.xF86XK_AudioLowerVolume), spawn "amixer -D pulse sset Master 5%-"),
      ((shiftMask, X11.xF86XK_AudioRaiseVolume), spawn "amixer -D pulse sset Master 1%+"),
      ((shiftMask, X11.xF86XK_AudioLowerVolume), spawn "amixer -D pulse sset Master 1%-"),
      ((noModMask, X11.xF86XK_AudioMute), spawn "amixer -D pulse sset Master toggle"),
      ((noModMask, X11.xF86XK_AudioMicMute), spawn "pactl set-source-mute 1 toggle"),
      ((superKey, xK_v), spawn "pavucontrol"),
      -- Screen shooter
      ((noModMask, xK_Print), spawn "xfce4-screenshooter"),
      ((superKey, xK_Print), spawn "xfce4-screenshooter -w"),
      ((controlMask, xK_Print), spawn "xfce4-screenshooter -r"),
      ((shiftMask, xK_Print), spawn "xfce4-screenshooter -f")
    ]
      <>
      -- mod-[1..9] %! Switch to workspace N
      -- mod-shift-[1..9] %! Move client to workspace N
      [ ((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9],
          (f, m) <- [(StackSet.greedyView, 0), (StackSet.shift, shiftMask)]
      ]
      <>
      -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
      -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
      [ ((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0 ..],
          (f, m) <- [(StackSet.view, 0), (StackSet.shift, shiftMask)]
      ]
      <> [ ((modMask .|. controlMask, k), windows $ Swap.swapWithCurrent i) -- Swap workspaces mod-ctrl-[1..9]
           | (i, k) <- zip (XMonad.workspaces conf) [xK_1 ..]
         ]

myStartupHook = for_ autoStartCommands SpawnOnce.spawnOnce
  where
    autoStartCommands =
      [ "mpv $XDG_CONFIG_HOME/dotfiles/assets/startup.mp3",
        "stalonetray -c $XDG_CONFIG_HOME/stalonetray/stalonetrayrc",
        "nm-applet",
        "xfce4-power-manager",
        "pasystray",
        "redshift-gtk",
        "blueman-applet",
        "/usr/lib/xfce4/notifyd/xfce4-notifyd",
        "lxsession"
      ]

myLayoutHook = hooks layout
  where
    hooks =
      Toggle.toggleLayouts Full
        >>> NoBorders.smartBorders
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
                { Tabbed.fontName = "xft:FreeSans:size=11",
                  Tabbed.activeColor = "#81A1C1",
                  Tabbed.activeBorderColor = "#81A1C1",
                  Tabbed.activeTextColor = "#3B4252",
                  Tabbed.activeBorderWidth = 0,
                  Tabbed.inactiveColor = "#3B4252",
                  Tabbed.inactiveBorderColor = "#3B4252",
                  Tabbed.inactiveTextColor = "#ECEFF4",
                  Tabbed.inactiveBorderWidth = 0,
                  Tabbed.urgentColor = "#BF616A",
                  Tabbed.urgentBorderColor = "#BF616A",
                  Tabbed.urgentBorderWidth = 0
                }

myLogHook handles = DynamicLog.dynamicLogWithPP myXmobarPp
  where
    myXmobarPp =
      def
        { DynamicLog.ppCurrent = xmobarColor "#EBCB8B" "" . wrap "[" "]",
          DynamicLog.ppTitle = xmobarColor "#A3BE8C" "" . shorten 40,
          DynamicLog.ppVisible = wrap "(" ")",
          DynamicLog.ppUrgent = xmobarColor "#BF616A" "#EBCB8B",
          DynamicLog.ppOutput = for_ handles . flip Run.hPutStrLn
        }
      where
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

myManageHook = className =? "Pavucontrol" --> ManageHelpers.doCenterFloat

makeConfig _ handles = hooks config'
  where
    hooks = Ewmh.ewmh >>> Ewmh.ewmhFullscreen >>> ManageDocks.docks
    config' =
      def
        { terminal = "alacritty",
          focusedBorderColor = "#81A1C1",
          normalBorderColor = "#3B4252",
          borderWidth = 3,
          keys = myKeys,
          layoutHook = myLayoutHook,
          logHook = myLogHook handles,
          manageHook = myManageHook,
          modMask = altKey,
          startupHook = myStartupHook
        }

main :: IO ()
main = do
  isDesktop <-
    Text.readFile "/etc/hostname"
      <&> Text.strip
      <&> (== "compe")
  alwaysHandles <- pure <$> Run.spawnPipe "xmobar-primary"
  handles <-
    if isDesktop
      then (: alwaysHandles) <$> Run.spawnPipe "xmobar-secondary"
      else pure alwaysHandles
  xmonad $ makeConfig isDesktop handles
