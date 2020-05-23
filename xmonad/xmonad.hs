{-# Language RecordWildCards #-}
{-# Language BangPatterns #-}

import           XMonad                   hiding ( (|||) )
import           XMonad.Actions.SwapWorkspaces   ( swapWithCurrent )
import           XMonad.Hooks.DynamicLog         ( xmobar )
import           XMonad.Hooks.EwmhDesktops       ( fullscreenEventHook, ewmh )
import           XMonad.Hooks.ManageDocks        ( avoidStruts )
import           XMonad.Layout.Grid              ( Grid(..) )
import           XMonad.Layout.LayoutCombinators ( JumpToLayout(..), (|||) )
import           XMonad.Layout.NoBorders         ( smartBorders )
import           XMonad.Util.Cursor              ( setDefaultCursor )
import           XMonad.Util.SpawnOnce           ( spawnOnce )

import           XMonad.Layout.Groups.Wmii


import qualified XMonad.Hooks.ManageHelpers    as MH
import qualified XMonad.StackSet               as W

import           Control.Monad
import           Data.Function
import           Data.Maybe
import           Data.Semigroup
import           Graphics.X11.ExtraTypes.XF86
import           System.Exit

import qualified Data.Map                      as M



-- Set variables for mod masks, since I can never remeber them.
superKey :: KeyMask
superKey = mod4Mask

altKey :: KeyMask
altKey = mod1Mask


-- | List of programs to spawn on start
programsToSpawn :: [String]
programsToSpawn =
  [ "redshift-gtk"
  , "xfce4-power-manager"
  , "feh --bg-center ~/.dotfiles/wallpaper/fields_of_utopia_2_nebula_by_starkiteckt-d8p2v1w.jpg"
  , "picom"
  , "stalonetray"
  , "nm-applet"
  , "blueman-applet"
  ]


-- | A hook for running programs at startup.
myStartupHook :: X ()
myStartupHook = do
  setDefaultCursor xC_left_ptr
  forM_ programsToSpawn spawnOnce


myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@XConfig {..} = M.fromList $
    -- launching and killing programs
    [ ((modMask,               xK_Return), spawn $ XMonad.terminal conf) -- %! Launch terminal
    , ((modMask,               xK_d     ), spawn "dmenu_run") -- %! Launch dmenu
    , ((superKey,              xK_Escape), kill) -- %! Close the focused window

    , ((modMask,               xK_space ), sendMessage NextLayout) -- %! Rotate through the available layout algorithms
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf) -- %!  Reset the layouts on the current workspace to default

    , ((modMask,               xK_n     ), refresh) -- %! Resize viewed windows to the correct size

    -- move focus up or down the window stack
    , ((modMask,               xK_Tab   ), windows W.focusDown) -- %! Move focus to the next window
    , ((modMask .|. shiftMask, xK_Tab   ), windows W.focusUp  ) -- %! Move focus to the previous window
    , ((modMask,               xK_j     ), windows W.focusDown) -- %! Move focus to the next window
    , ((modMask,               xK_k     ), windows W.focusUp  ) -- %! Move focus to the previous window
    , ((modMask,               xK_m     ), windows W.focusMaster  ) -- %! Move focus to the master window

    -- modifying the window order
    , ((modMask .|. shiftMask, xK_Return), windows W.swapMaster) -- %! Swap the focused window and the master window
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  ) -- %! Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    ) -- %! Swap the focused window with the previous window

    -- resizing the master/slave ratio
    , ((modMask,               xK_h     ), sendMessage Shrink) -- %! Shrink the master area
    , ((modMask,               xK_l     ), sendMessage Expand) -- %! Expand the master area

    -- floating layer support
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink) -- %! Push window back into tiling

    -- increase or decrease number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1)) -- %! Increment the number of windows in the master area
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1))) -- %! Deincrement the number of windows in the master area

    -- quit, or restart
    , ((modMask .|. shiftMask, xK_q     ), io exitSuccess) -- %! Quit xmonad
    , ((modMask              , xK_q     ), spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi") -- %! Restart xmonad

    -- Volume keys
    , ((0, xF86XK_AudioRaiseVolume), spawn "amixer -D pulse sset Master 5%+")
    , ((0, xF86XK_AudioLowerVolume), spawn "amixer -D pulse sset Master 5%-")
    , ((0, xF86XK_AudioMute)       , spawn "amixer -D pulse sset Master toggle")

    -- Change to specific layouts
    , ((modMask, xK_f), sendMessage $ JumpToLayout "Full")

    -- Autolaunch programs
    , ((superKey, xK_w), spawn "firefox")
    , ((superKey, xK_v), spawn "pavucontrol")
    , ((superKey, xK_f), spawn "thunar")
    ]
    <>
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    <>
    -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    <>
    -- Send contents of a workspace to another workspace
    [((modMask .|. controlMask, k), windows $ swapWithCurrent i)
        | (i, k) <- zip workspaces [xK_1 ..]]

myManageHook :: Query (Endo WindowSet)
myManageHook = composeAll 
  [ MH.isFullscreen --> MH.doFullFloat
  ] 

myLayoutHook =
  layouts
    & smartBorders
    & avoidStruts
 where
  nmaster = 1
  ratio   = 1 / 2
  delta   = 3 / 100
  tall    = Tall nmaster delta ratio
  layouts = tall ||| Mirror tall ||| Full ||| Grid ||| wmii shrinkText def


main :: IO ()
main = 
  xmonad =<< xmobar
    (ewmh $ def
      { focusedBorderColor = "#4c7899"
      , normalBorderColor  = "#333333"
      , borderWidth        = 3
      , terminal           = "alacritty"
      , modMask            = altKey
      , focusFollowsMouse  = True
      , startupHook        = myStartupHook >> addEWMHFullscreen
      , handleEventHook    = fullscreenEventHook
      , keys               = myKeys
      , manageHook         = myManageHook
      , layoutHook         = myLayoutHook
      }
    )
 where
  -- | Fixes full screen issues with firefox... no clue how it works.
  -- Source: https://github.com/evanjs/gentoo-dotfiles/commit/cbf78364ea60e62466594340090d8e99200e8e08
  addEWMHFullscreen = do
    wms <- getAtom "_NET_WM_STATE"
    wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
    mapM_ addNETSupported [wms, wfs]
   where
    addNETSupported !x = withDisplay $ \(!dpy) -> do
      r               <- asks theRoot
      a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
      a               <- getAtom "ATOM"
      liftIO $ do
        sup <- join . maybeToList <$> getWindowProperty32 dpy a_NET_SUPPORTED r
        when (fromIntegral x `notElem` sup) $ changeProperty32
          dpy
          r
          a_NET_SUPPORTED
          a
          propModeAppend
          [fromIntegral x]

-- vim: set nowrap:
