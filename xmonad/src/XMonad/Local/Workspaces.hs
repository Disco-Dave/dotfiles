module XMonad.Local.Workspaces (
  Workspace (..),
  workspaces,
  workspaceNames,
  workspaceKeys,
) where

import qualified Graphics.X11 as X11

data Workspace = Workspace
  { workspaceName :: String
  , workspaceKey :: X11.KeySym
  }
  deriving (Show)

workspaces :: [Workspace]
workspaces =
  let names = show @Int <$> [1 ..]
      keys = [X11.xK_1 .. X11.xK_9] <> [X11.xK_0]
   in zipWith Workspace names keys

workspaceNames :: [String]
workspaceNames =
  fmap workspaceName workspaces

workspaceKeys :: [X11.KeySym]
workspaceKeys =
  fmap workspaceKey workspaces
