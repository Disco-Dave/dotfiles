module Main (main) where

import qualified Control.Monad.Reader as Reader
import qualified Data.Map.Strict as Map
import qualified XMonad
import XMonad.Local.Environment (Environment (envTheme), getEnvironment)
import XMonad.Local.Hooks (applyHooks)
import qualified XMonad.Local.Keys as Keys
import qualified XMonad.Local.Layout as LayoutHook
import qualified XMonad.Local.ManageHook as ManageHook
import qualified XMonad.Local.StartupHook as StartupHook
import qualified XMonad.Local.Theme as Theme
import qualified XMonad.Local.Theme.Color as Color
import XMonad.Local.Workspaces (workspaceNames)

main :: IO ()
main = do
  env <- getEnvironment

  let runReaderT =
        flip Reader.runReaderT env
      runReader =
        flip Reader.runReader env

  let start config =
        runReaderT (applyHooks config) >>= XMonad.xmonad

  start $
    let color windowColor =
          let theme = envTheme env
              windowTheme = Theme.themeWindow theme
           in Color.toString $ windowColor windowTheme
     in XMonad.def
          { XMonad.terminal = "alacritty"
          , XMonad.borderWidth = 3
          , XMonad.focusedBorderColor = color Theme.windowBorderFocussed
          , XMonad.normalBorderColor = color Theme.windowBorder
          , XMonad.modMask = Keys.altKey
          , XMonad.keys = Map.map runReaderT . Keys.keys
          , XMonad.manageHook = runReader ManageHook.manageHook
          , XMonad.startupHook = runReaderT StartupHook.startupHook
          , XMonad.workspaces = workspaceNames
          , XMonad.layoutHook = runReader LayoutHook.layoutHook
          }
