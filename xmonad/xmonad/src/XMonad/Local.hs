module XMonad.Local (start) where

import Control.Monad.Reader qualified as Reader
import Data.Map.Strict qualified as Map
import Shared.Environment (getEnvironment)
import Shared.Environment qualified as Environment
import Shared.Theme qualified as Theme
import Shared.Theme.Color qualified as Color
import XMonad qualified
import XMonad.Local.Hooks (applyHooks)
import XMonad.Local.Keys qualified as Keys
import XMonad.Local.Layout qualified as LayoutHook
import XMonad.Local.ManageHook qualified as ManageHook
import XMonad.Local.StartupHook qualified as StartupHook
import XMonad.Local.Workspaces (workspaceNames)


start :: IO ()
start = do
  env <- getEnvironment

  let runReaderT =
        flip Reader.runReaderT env
      runReader =
        flip Reader.runReader env

  let startXmonad config =
        runReaderT (applyHooks config) >>= XMonad.xmonad

  startXmonad $
    let color windowColor =
          let theme = env.theme
           in Color.toHashString $ windowColor theme.window
        hostname = env.hostname
     in XMonad.def
          { XMonad.terminal = "alacritty"
          , XMonad.borderWidth = 3
          , XMonad.focusedBorderColor = color (.borderFocussed)
          , XMonad.normalBorderColor = color (.border)
          , XMonad.modMask = Keys.altKey
          , XMonad.keys = Map.map runReaderT . Keys.keys hostname
          , XMonad.manageHook = runReader ManageHook.manageHook
          , XMonad.startupHook = runReaderT StartupHook.startupHook
          , XMonad.workspaces = workspaceNames hostname
          , XMonad.layoutHook = runReader LayoutHook.layoutHook
          }
