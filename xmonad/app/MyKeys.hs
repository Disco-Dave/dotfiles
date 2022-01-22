module MyKeys (
  superKey,
  altKey,
  keys,
) where

import qualified Data.Map.Strict as Map
import qualified Graphics.X11.ExtraTypes as X11
import System.Exit (exitSuccess)
import XMonad hiding (keys)
import qualified XMonad.Actions.SwapWorkspaces as Swap
import qualified XMonad.Hooks.ManageDocks as ManageDocks
import qualified XMonad.Layout.ToggleLayouts as Toggle
import qualified XMonad.StackSet as StackSet

superKey :: KeyMask
superKey = mod4Mask

altKey :: KeyMask
altKey = mod1Mask

spawnDmenu :: X ()
spawnDmenu = do
  (S screenId) <- gets $ StackSet.screen . StackSet.current . windowset
  spawn $
    "dmenu_run -fn \"FreeSans-12\" -nb \"#3B4252\" -sb \"#81A1C1\" -nf \"#ECEFF4\" -sf \"#3B4252\" -m "
      <> show screenId

spawnPassmenu :: X ()
spawnPassmenu = do
  (S screenId) <- gets $ StackSet.screen . StackSet.current . windowset
  spawn $
    "passmenu -fn \"FreeSans-12\" -nb \"#3B4252\" -sb \"#81A1C1\" -nf \"#ECEFF4\" -sf \"#3B4252\" -m "
      <> show screenId

keys conf@XConfig{XMonad.modMask = modMask} =
  Map.fromList $
    -- launching and killing programs
    [ ((modMask, xK_Return), spawn $ XMonad.terminal conf) -- %! Launch terminal
    , ((modMask .|. shiftMask, xK_c), kill) -- %! Close the focused window
    , ((superKey, xK_Escape), kill)
    , ((modMask, xK_p), spawnDmenu)
    , ((modMask .|. shiftMask, xK_p), spawnPassmenu)
    , ((superKey, xK_w), spawn "firefox")
    , ((superKey, xK_e), spawn "birdtray -t")
    , -- changing layout commands
      ((modMask, xK_space), sendMessage NextLayout) -- %! Rotate through the available layout algorithms
    , ((modMask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf) -- %!  Reset the layouts on the current workspace to default
    , ((modMask, xK_f), sendMessage Toggle.ToggleLayout) -- %! Toggle the Full layout
    , ((modMask, xK_n), refresh) -- %! Resize viewed windows to the correct size
    , ((modMask, xK_s), sendMessage ManageDocks.ToggleStruts) -- move focus up or down the window stack
    , ((modMask, xK_Tab), windows StackSet.focusDown) -- %! Move focus to the next window
    , ((modMask .|. shiftMask, xK_Tab), windows StackSet.focusUp) -- %! Move focus to the previous window
    , ((modMask, xK_j), windows StackSet.focusDown) -- %! Move focus to the next window
    , ((modMask, xK_k), windows StackSet.focusUp) -- %! Move focus to the previous window
    , ((modMask, xK_m), windows StackSet.focusMaster) -- %! Move focus to the master window
    , -- modifying the window order
      ((modMask .|. shiftMask, xK_Return), windows StackSet.swapMaster) -- %! Swap the focused window and the master window
    , ((modMask .|. shiftMask, xK_j), windows StackSet.swapDown) -- %! Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_k), windows StackSet.swapUp) -- %! Swap the focused window with the previous window
    , -- resizing the master/slave ratio
      ((modMask, xK_h), sendMessage Shrink) -- %! Shrink the master area
    , ((modMask, xK_l), sendMessage Expand) -- %! Expand the master area
    , -- floating layer support
      ((modMask, xK_t), withFocused $ windows . StackSet.sink) -- %! Push window back into tiling
    , -- increase or decrease number of windows in the master area
      ((modMask, xK_comma), sendMessage (IncMasterN 1)) -- %! Increment the number of windows in the master area
    , ((modMask, xK_period), sendMessage (IncMasterN (-1))) -- %! Deincrement the number of windows in the master area
    , -- quit, or restart
      ((modMask .|. shiftMask, xK_q), io exitSuccess) -- %! Quit xmonad
    , ((modMask, xK_q), spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi") -- %! Restart xmonad
    , -- Volume keys
      ((noModMask, X11.xF86XK_AudioRaiseVolume), spawn "amixer -D pulse sset Master 5%+")
    , ((noModMask, X11.xF86XK_AudioLowerVolume), spawn "amixer -D pulse sset Master 5%-")
    , ((shiftMask, X11.xF86XK_AudioRaiseVolume), spawn "amixer -D pulse sset Master 1%+")
    , ((shiftMask, X11.xF86XK_AudioLowerVolume), spawn "amixer -D pulse sset Master 1%-")
    , ((noModMask, X11.xF86XK_AudioMute), spawn "amixer -D pulse sset Master toggle")
    , ((noModMask, X11.xF86XK_AudioMicMute), spawn "pactl set-source-mute 1 toggle")
    , ((superKey, xK_v), spawn "pavucontrol")
    , -- MPC keys
      ((noModMask, X11.xF86XK_AudioNext), spawn "mpc next")
    , ((noModMask, X11.xF86XK_AudioPrev), spawn "mpc prev")
    , ((noModMask, X11.xF86XK_AudioStop), spawn "mpc stop")
    , ((noModMask, X11.xF86XK_AudioPlay), spawn "mpc toggle")
    , -- Screen shooter
      ((noModMask, xK_Print), spawn "xfce4-screenshooter")
    , ((superKey, xK_Print), spawn "xfce4-screenshooter -w")
    , ((controlMask, xK_Print), spawn "xfce4-screenshooter -r")
    , ((shiftMask, xK_Print), spawn "xfce4-screenshooter -f")
    ]
      <>
      -- mod-[1..9] %! Switch to workspace N
      -- mod-shift-[1..9] %! Move client to workspace N
      [ ((m .|. modMask, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
      , (f, m) <- [(StackSet.greedyView, 0), (StackSet.shift, shiftMask)]
      ]
      <>
      -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
      -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
      [ ( (m .|. modMask, key)
        , screenWorkspace sc >>= flip whenJust (windows . f)
        )
      | (key, sc) <- zip [xK_w, xK_e, xK_r] [0 ..]
      , (f, m) <- [(StackSet.view, 0), (StackSet.shift, shiftMask)]
      ]
      <> [ ((modMask .|. controlMask, k), windows $ Swap.swapWithCurrent i) -- Swap workspaces mod-ctrl-[1..9]
         | (i, k) <- zip (XMonad.workspaces conf) [xK_1 ..]
         ]
