module XMonad.Local.Workspaces (
  Workspace (..),
  workspaces,
  workspaceNames,
  workspaceKeys,
) where

import qualified Graphics.X11 as X11
import XMonad.Local.Hostname (Hostname)
import qualified XMonad.Local.Hostname as Hostname

data Workspace = Workspace
  { workspaceName :: String
  , workspaceKey :: X11.KeySym
  }
  deriving (Show)

workspaces :: Hostname -> [Workspace]
workspaces hostname =
  let threeMonitors =
        let names = show @Int <$> ([2, 1] <> [3 ..])
            keys = [X11.xK_2, X11.xK_1] <> [X11.xK_3 .. X11.xK_9] <> [X11.xK_0]
         in zipWith Workspace names keys
   in case hostname of
        Hostname.Desktop -> threeMonitors
        Hostname.Work -> threeMonitors
        _ ->
          let names = show @Int <$> [1 ..]
              keys = [X11.xK_1 .. X11.xK_9] <> [X11.xK_0]
           in zipWith Workspace names keys

workspaceNames :: Hostname -> [String]
workspaceNames hostname =
  fmap workspaceName (workspaces hostname)

workspaceKeys :: Hostname -> [X11.KeySym]
workspaceKeys hostname =
  fmap workspaceKey (workspaces hostname)
