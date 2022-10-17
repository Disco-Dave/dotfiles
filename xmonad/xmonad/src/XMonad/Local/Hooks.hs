module XMonad.Local.Hooks
  ( applyHooks
  )
where

import Control.Monad.Reader (ReaderT)
import Data.Map.Strict qualified as Map
import Graphics.X11 qualified as X11
import Shared.Environment (Environment)
import XMonad (def)
import XMonad qualified
import XMonad.Actions.FlexibleResize qualified as FlexibleResize
import XMonad.Actions.Navigation2D qualified as Nav2D
import XMonad.Hooks.EwmhDesktops qualified as Ewmh
import XMonad.Hooks.ManageDocks qualified as ManageDocks
import XMonad.Local.StatusBar qualified as StatusBar


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
        , Nav2D.withNavigation2DConfig def
        ]
   in StatusBar.addStatusBar $ foldr ($) config hooks
