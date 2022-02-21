module XMonad.Local.Hooks (
  applyHooks,
) where

import Control.Monad.Reader (ReaderT)
import qualified Data.Map.Strict as Map
import qualified Graphics.X11 as X11
import qualified XMonad
import qualified XMonad.Actions.FlexibleResize as FlexibleResize
import qualified XMonad.Hooks.EwmhDesktops as Ewmh
import qualified XMonad.Hooks.ManageDocks as ManageDocks
import XMonad.Local.Environment (Environment)
import qualified XMonad.Local.StatusBar as StatusBar

resizeByAnyCorner :: XMonad.XConfig l -> XMonad.XConfig l
resizeByAnyCorner oldConfig@XMonad.XConfig{modMask} =
  let newKey = (modMask, X11.button3)
      newAction w = XMonad.focus w *> FlexibleResize.mouseResizeWindow w
   in oldConfig
        { XMonad.mouseBindings = Map.insert newKey newAction . XMonad.mouseBindings oldConfig
        }

applyHooks :: (XMonad.LayoutClass l XMonad.Window) => XMonad.XConfig l -> ReaderT Environment IO (XMonad.XConfig l)
applyHooks config =
  let hooks =
        [ Ewmh.ewmh
        , Ewmh.ewmhFullscreen
        , ManageDocks.docks
        , resizeByAnyCorner
        ]
   in StatusBar.addStatusBar $ foldr ($) config hooks
