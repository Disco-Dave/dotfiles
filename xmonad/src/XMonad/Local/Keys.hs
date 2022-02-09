module XMonad.Local.Keys (
  superKey,
  altKey,
  keys,
) where

import Control.Monad.Reader (ReaderT)
import qualified Control.Monad.Reader as Reader
import Control.Monad.Trans (lift)
import Data.Bits ((.|.))
import Data.Foldable (foldl')
import Data.Map (Map)
import qualified Data.Map.Merge.Strict as Merge
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Graphics.X11 as X11
import qualified Graphics.X11.ExtraTypes as X11
import XMonad (X)
import qualified XMonad
import qualified XMonad.Actions.SwapWorkspaces as Swap
import XMonad.Local.Environment (Environment (..))
import qualified XMonad.Local.Theme as Theme
import qualified XMonad.Local.Theme.Color as Color
import qualified XMonad.Local.Theme.Font as Font
import qualified XMonad.Local.Utils as Utils
import qualified XMonad.StackSet as StackSet

superKey :: X11.KeyMask
superKey = X11.mod4Mask

altKey :: X11.KeyMask
altKey = X11.mod1Mask

dmenuFlags :: ReaderT Environment X String
dmenuFlags = do
  theme <- Reader.asks envTheme

  screenId <- lift Utils.currentScreenId

  let font = Theme.themeFont theme
      dmenuTheme = Theme.themeDmenu theme

  let flag name value =
        name <> " " <> "\"" <> value <> "\""

  let color selectColor =
        Color.toString $ selectColor dmenuTheme

  pure . unwords $
    [ flag "-fn" $
        let family = Text.unpack $ Font.fontFamily font
            size = show $ Font.fontSize font
         in family <> "-" <> size
    , flag "-nb" (color Theme.dmenuNormalBackground)
    , flag "-sb" (color Theme.dmenuSelectedBackground)
    , flag "-nf" (color Theme.dmenuNormalForeground)
    , flag "-sf" (color Theme.dmenuSelectedForeground)
    , flag "-m" (show screenId)
    ]

spawnDmenu :: String -> ReaderT Environment X ()
spawnDmenu command = do
  flags <- dmenuFlags
  XMonad.spawn $ command <> flags

type KeyMap =
  forall l. XMonad.XConfig l -> Map (X11.KeyMask, X11.KeySym) (ReaderT Environment X ())

applicationShortcuts :: KeyMap
applicationShortcuts XMonad.XConfig{modMask, terminal} =
  Map.fromList
    [ ((superKey, X11.xK_Escape), lift XMonad.kill)
    , ((modMask, X11.xK_Return), XMonad.spawn terminal)
    , ((superKey, X11.xK_w), XMonad.spawn "firefox")
    , ((superKey, X11.xK_e), XMonad.spawn "birdtray -t")
    , ((superKey, X11.xK_v), XMonad.spawn "pavucontrol")
    , ((superKey, X11.xK_f), XMonad.spawn "thunar")
    , ((superKey, X11.xK_v), XMonad.spawn "pavucontrol")
    ]

dmenus :: KeyMap
dmenus XMonad.XConfig{modMask} =
  Map.fromList
    [ ((modMask, X11.xK_p), spawnDmenu "dmenu_run")
    , ((modMask .|. X11.shiftMask, X11.xK_p), spawnDmenu "passmenu")
    ]

screenshots :: KeyMap
screenshots _ =
  Map.fromList
    [ ((X11.noModMask, X11.xK_Print), XMonad.spawn "xfce4-screenshooter")
    , ((superKey, X11.xK_Print), XMonad.spawn "xfce4-screenshooter -w")
    , ((X11.controlMask, X11.xK_Print), XMonad.spawn "xfce4-screenshooter -r")
    , ((X11.shiftMask, X11.xK_Print), XMonad.spawn "xfce4-screenshooter -f")
    ]

media :: KeyMap
media _ =
  Map.fromList
    [ ((X11.noModMask, X11.xF86XK_AudioRaiseVolume), XMonad.spawn "amixer -D pulse sset Master 5%+")
    , ((X11.noModMask, X11.xF86XK_AudioLowerVolume), XMonad.spawn "amixer -D pulse sset Master 5%-")
    , ((X11.shiftMask, X11.xF86XK_AudioRaiseVolume), XMonad.spawn "amixer -D pulse sset Master 1%+")
    , ((X11.shiftMask, X11.xF86XK_AudioLowerVolume), XMonad.spawn "amixer -D pulse sset Master 1%-")
    , ((X11.noModMask, X11.xF86XK_AudioMute), XMonad.spawn "amixer -D pulse sset Master toggle")
    , ((X11.noModMask, X11.xF86XK_AudioMicMute), XMonad.spawn "pactl set-source-mute 1 toggle")
    , ((X11.noModMask, X11.xF86XK_AudioNext), XMonad.spawn "mpc next")
    , ((X11.noModMask, X11.xF86XK_AudioPrev), XMonad.spawn "mpc prev")
    , ((X11.noModMask, X11.xF86XK_AudioStop), XMonad.spawn "mpc stop")
    , ((X11.noModMask, X11.xF86XK_AudioPlay), XMonad.spawn "mpc toggle")
    ]

workspaces :: KeyMap
workspaces XMonad.XConfig{modMask, workspaces = workspaceNames} =
  Map.fromList $
    zip workspaceNames [X11.xK_1 ..] >>= \(workspaceId, key) ->
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

screens :: KeyMap
screens XMonad.XConfig{modMask} =
  Map.fromList $
    zip [0 ..] [X11.xK_w, X11.xK_e, X11.xK_r] >>= \(screenId, key) ->
      let modifyWindowSet f = do
            maybeWorkspace <- lift $ XMonad.screenWorkspace screenId
            case maybeWorkspace of
              Nothing -> pure ()
              Just workspace ->
                lift . XMonad.windows $ f workspace
       in [
            ( (modMask, key) -- Focus screen
            , modifyWindowSet StackSet.greedyView
            )
          ,
            ( (modMask .|. X11.shiftMask, key) -- Move window to screen
            , modifyWindowSet StackSet.shift
            )
          ]

keys :: KeyMap
keys conf =
  let allKeyMaps =
        [ dmenus
        , applicationShortcuts
        , screenshots
        , media
        , workspaces
        , screens
        ]
      merge oldMap keyMap =
        Merge.merge
          Merge.preserveMissing
          Merge.preserveMissing
          (Merge.zipWithMatched (\_ _ action -> action))
          oldMap
          (keyMap conf)
   in foldl' merge Map.empty allKeyMaps
