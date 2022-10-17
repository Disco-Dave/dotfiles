module XMonad.Local.Workspaces
  ( Workspace (..)
  , workspaces
  , workspaceNames
  , workspaceKeys
  )
where

import Graphics.X11 qualified as X11
import Shared.Hostname (Hostname)
import Shared.Hostname qualified as Hostname


data Workspace = Workspace
  { name :: String
  , key :: X11.KeySym
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
  fmap (.name) (workspaces hostname)


workspaceKeys :: Hostname -> [X11.KeySym]
workspaceKeys hostname =
  fmap (.key) (workspaces hostname)
