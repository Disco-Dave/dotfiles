module XMonad.Local.Workspaces (
  workspaces,
) where

import Control.Monad.Reader (ReaderT)
import XMonad (X)
import XMonad.Hooks.StatusBar.PP as PP
import XMonad.Local.Environment (Environment)

workspaces :: ReaderT Environment X [String]
workspaces =
  let toWorkspaceId index =
        PP.xmobarAction ("xdotool key alt+" <> index) "1" index
      workspaceIds =
        fmap (toWorkspaceId . show) [1 .. 9 :: Integer]
   in pure workspaceIds
