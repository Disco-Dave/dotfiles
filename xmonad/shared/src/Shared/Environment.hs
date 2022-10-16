module Shared.Environment
  ( Environment (..)
  , getEnvironment
  )
where

import Shared.FilePaths (FilePaths, getFilePaths)
import Shared.Hostname (Hostname, getHostname)
import Shared.Theme (Theme)
import Shared.Theme qualified as Theme


data Environment = Environment
  { theme :: Theme
  , hostname :: Hostname
  , filePaths :: FilePaths
  }
  deriving (Show, Eq)


getEnvironment :: IO Environment
getEnvironment = do
  let theme = Theme.nord

  hostname <- getHostname
  filePaths <- getFilePaths

  pure Environment{..}
