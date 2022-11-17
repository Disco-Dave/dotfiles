module XMonad.Local.Keys
  ( superKey
  , altKey
  , keys
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Reader qualified as Reader
import Control.Monad.Trans (lift)
import Data.Bits ((.|.))
import Data.Map (Map)
import Data.Map.Merge.Strict qualified as Merge
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Graphics.X11 qualified as X11
import Graphics.X11.ExtraTypes qualified as X11
import Shared.Environment (Environment (..))
import Shared.Hostname (Hostname)
import Shared.Hostname qualified as Hostname
import Shared.Theme qualified as Theme
import Shared.Theme.Color qualified as Color
import Shared.Theme.Font qualified as Font
import System.Exit (exitSuccess)
import XMonad (X)
import XMonad qualified
import XMonad.Actions.Navigation2D qualified as Nav2D
import XMonad.Actions.SwapWorkspaces qualified as Swap
import XMonad.Hooks.ManageDocks qualified as ManageDocks
import XMonad.Layout.BinarySpacePartition qualified as Bsp
import XMonad.Layout.MultiToggle (Toggle (Toggle))
import XMonad.Layout.Reflect (REFLECTX (REFLECTX), REFLECTY (REFLECTY))
import XMonad.Layout.ToggleLayouts qualified as Toggle
import XMonad.Local.Layout (LayoutName)
import XMonad.Local.Layout qualified as Layout
import XMonad.Local.Layout qualified as LayoutName
import XMonad.Local.Utils qualified as Utils
import XMonad.Local.Workspaces (workspaceKeys)
import XMonad.StackSet qualified as StackSet


superKey :: X11.KeyMask
superKey = X11.mod4Mask


altKey :: X11.KeyMask
altKey = X11.mod1Mask


dmenuFlags :: ReaderT Environment X String
dmenuFlags = do
  theme <- Reader.asks (.theme)
  hostname <- Reader.asks (.hostname)

  screenId <- lift Utils.currentScreenId

  let font = theme.font
      dmenuTheme = theme.dmenu

  let flag name value =
        name <> " " <> "\"" <> value <> "\""

  let color selectColor =
        Color.toHashString $ selectColor dmenuTheme

  pure . unwords $
    [ flag "-fn" $
        let family = Text.unpack font.name
            size = show $ case hostname of
              Hostname.Laptop -> font.size
              _ -> 14
         in family <> "-" <> size
    , flag "-nb" (color (.normalBackground))
    , flag "-sb" (color (.selectedBackground))
    , flag "-nf" (color (.normalForeground))
    , flag "-sf" (color (.selectedForeground))
    , flag "-m" (show screenId)
    ]


spawnDmenu :: String -> ReaderT Environment X ()
spawnDmenu command = do
  flags <- dmenuFlags
  XMonad.spawn $ command <> " " <> flags


type KeyMap =
  XMonad.XConfig XMonad.Layout -> Map (X11.KeyMask, X11.KeySym) (ReaderT Environment X ())


applicationShortcuts :: KeyMap
applicationShortcuts XMonad.XConfig{modMask, terminal} =
  Map.fromList
    [ ((superKey, X11.xK_Escape), lift XMonad.kill)
    , ((modMask, X11.xK_Return), XMonad.spawn terminal)
    , ((superKey, X11.xK_w), XMonad.spawn "firefox")
    , ((superKey, X11.xK_e), XMonad.spawn "birdtray -t")
    , ((superKey, X11.xK_v), XMonad.spawn "pavucontrol")
    , ((superKey, X11.xK_f), XMonad.spawn "thunar")
    ]


dmenuKeyMap :: KeyMap
dmenuKeyMap XMonad.XConfig{modMask} =
  Map.fromList
    [ ((modMask, X11.xK_p), spawnDmenu "dmenu_run")
    , ((modMask .|. X11.shiftMask, X11.xK_p), spawnDmenu "passmenu")
    ]


screenshotKeyMap :: KeyMap
screenshotKeyMap _ =
  Map.fromList
    [ ((X11.noModMask, X11.xK_Print), XMonad.spawn "xfce4-screenshooter")
    , ((superKey, X11.xK_Print), XMonad.spawn "xfce4-screenshooter -w")
    , ((X11.controlMask, X11.xK_Print), XMonad.spawn "xfce4-screenshooter -r")
    , ((X11.shiftMask, X11.xK_Print), XMonad.spawn "xfce4-screenshooter -f")
    ]


mediaKeyMap :: KeyMap
mediaKeyMap _ =
  Map.fromList
    [ ((X11.noModMask, X11.xF86XK_AudioRaiseVolume), XMonad.spawn "amixer -D pulse sset Master 5%+")
    , ((X11.noModMask, X11.xF86XK_AudioLowerVolume), XMonad.spawn "amixer -D pulse sset Master 5%-")
    , ((X11.shiftMask, X11.xF86XK_AudioRaiseVolume), XMonad.spawn "amixer -D pulse sset Master 1%+")
    , ((X11.shiftMask, X11.xF86XK_AudioLowerVolume), XMonad.spawn "amixer -D pulse sset Master 1%-")
    , ((X11.noModMask, X11.xF86XK_AudioMute), XMonad.spawn "amixer -D pulse sset Master toggle")
    , ((X11.noModMask, X11.xF86XK_AudioMicMute), XMonad.spawn "pactl set-source-mute 1 toggle")
    , ((X11.noModMask, X11.xF86XK_AudioNext), XMonad.spawn "mpc next")
    , ((X11.noModMask, X11.xF86XK_AudioPrev), XMonad.spawn "mpc prev")
    , ((X11.noModMask .|. X11.shiftMask, X11.xF86XK_AudioNext), XMonad.spawn "mpc seek +5%")
    , ((X11.noModMask .|. X11.shiftMask, X11.xF86XK_AudioPrev), XMonad.spawn "mpc seek -5%")
    , ((X11.noModMask, X11.xF86XK_AudioStop), XMonad.spawn "mpc stop")
    , ((X11.noModMask .|. X11.shiftMask, X11.xF86XK_AudioPlay), XMonad.spawn "mpc stop")
    , ((X11.noModMask, X11.xF86XK_AudioPlay), XMonad.spawn "mpc toggle")
    ]


workspaceKeyMap :: Hostname -> KeyMap
workspaceKeyMap hostname XMonad.XConfig{modMask, workspaces = workspaceNames} =
  Map.fromList $
    zip workspaceNames (workspaceKeys hostname) >>= \(workspaceId, key) ->
      let modifyWindowSet f = lift $ XMonad.windows (f workspaceId)
       in [
            ( (modMask, key) -- Focus workspace
            , modifyWindowSet StackSet.greedyView
            )
          ,
            ( (modMask .|. X11.shiftMask, key) -- Move window to workspace
            , modifyWindowSet StackSet.shift
            )
          ,
            ( (modMask .|. X11.controlMask, key) -- Swap workspace with current
            , modifyWindowSet Swap.swapWithCurrent
            )
          ]


screenKeyMap :: KeyMap
screenKeyMap XMonad.XConfig{modMask} =
  Map.fromList $
    [(1, X11.xK_q), (0, X11.xK_w), (2, X11.xK_e)] >>= \(screenId, key) ->
      let modifyWindowSet f = do
            maybeWorkspace <- lift $ XMonad.screenWorkspace screenId
            case maybeWorkspace of
              Nothing -> pure ()
              Just workspace ->
                lift . XMonad.windows $ f workspace
       in [
            ( (modMask, key) -- Focus screen
            , modifyWindowSet StackSet.view
            )
          ,
            ( (modMask .|. X11.shiftMask, key) -- Move window to screen
            , modifyWindowSet StackSet.shift
            )
          ,
            ( (modMask .|. X11.controlMask, key)
            , modifyWindowSet StackSet.greedyView
            )
          ]


layoutKeyMap :: KeyMap
layoutKeyMap conf@XMonad.XConfig{modMask} =
  Map.fromList
    [ ((modMask, X11.xK_space), lift $ XMonad.sendMessage XMonad.NextLayout) -- %! Rotate through the available layout algorithms
    , ((modMask .|. X11.shiftMask, X11.xK_space), lift . XMonad.setLayout $ XMonad.layoutHook conf) -- %!  Reset the layouts on the current workspace to default
    , ((modMask, X11.xK_f), lift $ XMonad.sendMessage Toggle.ToggleLayout) -- %! Toggle the Full layout
    , ((modMask, X11.xK_n), lift XMonad.refresh) -- %! Resize viewed windows to the correct size
    , ((modMask .|. X11.shiftMask, X11.xK_s), lift $ XMonad.sendMessage ManageDocks.ToggleStruts) -- move focus up or down the window stack
    , ((modMask, X11.xK_j), lift $ XMonad.windows StackSet.focusDown) -- %! Move focus to the next window
    , ((modMask, X11.xK_k), lift $ XMonad.windows StackSet.focusUp) -- %! Move focus to the previous window
    , ((modMask, X11.xK_m), lift $ XMonad.windows StackSet.focusMaster) -- %! Move focus to the master window
    , ((modMask .|. X11.shiftMask, X11.xK_Return), lift $ XMonad.windows StackSet.swapMaster) -- %! Swap the focused window and the master window
    , ((modMask .|. X11.shiftMask, X11.xK_j), lift $ XMonad.windows StackSet.swapDown) -- %! Swap the focused window with the next window
    , ((modMask .|. X11.shiftMask, X11.xK_k), lift $ XMonad.windows StackSet.swapUp) -- %! Swap the focused window with the previous window
    , ((modMask, X11.xK_h), lift $ XMonad.sendMessage XMonad.Shrink) -- %! Shrink the master area
    , ((modMask, X11.xK_l), lift $ XMonad.sendMessage XMonad.Expand) -- %! Expand the master area
    , ((modMask, X11.xK_t), lift . XMonad.withFocused $ XMonad.windows . StackSet.sink) -- %! Push window back into tiling
    , ((modMask, X11.xK_comma), lift $ XMonad.sendMessage (XMonad.IncMasterN 1)) -- %! Increment the number of windows in the master area
    , ((modMask, X11.xK_period), lift $ XMonad.sendMessage (XMonad.IncMasterN (-1))) -- %! Deincrement the number of windows in the master area
    , ((modMask, X11.xK_x), lift . XMonad.sendMessage $ Toggle REFLECTX)
    ]


quitAndRestartKeyMap :: KeyMap
quitAndRestartKeyMap XMonad.XConfig{modMask} =
  Map.fromList
    [ ((modMask .|. X11.shiftMask, X11.xK_Tab), liftIO exitSuccess) -- %! Quit xmonad
    , ((modMask, X11.xK_Tab), XMonad.spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi") -- %! Restart xmonad
    ]


merge :: Ord k => Map k a -> Map k a -> Map k a
merge =
  Merge.merge
    Merge.preserveMissing
    Merge.preserveMissing
    (Merge.zipWithMatched (\_ _ new -> new))


defaultKeys :: Hostname -> KeyMap
defaultKeys hostname conf =
  let allKeyMaps =
        fmap
          ($ conf)
          [ dmenuKeyMap
          , applicationShortcuts
          , screenshotKeyMap
          , mediaKeyMap
          , workspaceKeyMap hostname
          , screenKeyMap
          , layoutKeyMap
          , quitAndRestartKeyMap
          ]
   in foldr (flip merge) Map.empty allKeyMaps


layoutOverrides :: LayoutName -> KeyMap
layoutOverrides layoutName XMonad.XConfig{modMask} =
  Map.fromList $ case layoutName of
    LayoutName.Bsp ->
      [ ((modMask, X11.xK_l), lift $ Nav2D.windowGo Nav2D.R False)
      , ((modMask, X11.xK_h), lift $ Nav2D.windowGo Nav2D.L False)
      , ((modMask, X11.xK_k), lift $ Nav2D.windowGo Nav2D.U False)
      , ((modMask, X11.xK_j), lift $ Nav2D.windowGo Nav2D.D False)
      , ((modMask .|. X11.shiftMask, X11.xK_l), lift $ Nav2D.windowSwap Nav2D.R False)
      , ((modMask .|. X11.shiftMask, X11.xK_h), lift $ Nav2D.windowSwap Nav2D.L False)
      , ((modMask .|. X11.shiftMask, X11.xK_k), lift $ Nav2D.windowSwap Nav2D.U False)
      , ((modMask .|. X11.shiftMask, X11.xK_j), lift $ Nav2D.windowSwap Nav2D.D False)
      , ((modMask .|. X11.controlMask, X11.xK_l), lift . XMonad.sendMessage $ Bsp.ExpandTowards Bsp.R)
      , ((modMask .|. X11.controlMask, X11.xK_h), lift . XMonad.sendMessage $ Bsp.ExpandTowards Bsp.L)
      , ((modMask .|. X11.controlMask, X11.xK_j), lift . XMonad.sendMessage $ Bsp.ExpandTowards Bsp.D)
      , ((modMask .|. X11.controlMask, X11.xK_k), lift . XMonad.sendMessage $ Bsp.ExpandTowards Bsp.U)
      , ((modMask, X11.xK_r), lift $ XMonad.sendMessage Bsp.Rotate)
      , ((modMask, X11.xK_s), lift $ XMonad.sendMessage Bsp.Swap)
      ]
    LayoutName.Wide ->
      [ ((modMask, X11.xK_x), lift . XMonad.sendMessage $ Toggle REFLECTY)
      ]
    _ -> []


keys :: Hostname -> KeyMap
keys hostname conf =
  let layoutNames = [minBound @LayoutName ..]
      allKeys = foldMap (Set.fromList . Map.keys . flip layoutOverrides conf) layoutNames
      doNothingMap = Map.fromList . fmap (,pure ()) $ Set.toList allKeys
      overlay binding originalCommand = do
        layoutName <- lift Layout.getLayoutName
        let overrides = layoutOverrides layoutName conf
        fromMaybe originalCommand (Map.lookup binding overrides)
   in Map.mapWithKey overlay $ merge doNothingMap (defaultKeys hostname conf)
