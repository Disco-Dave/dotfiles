module Shared.FilePaths
  ( FilePaths (..)
  , getFilePaths
  )
where

import System.Directory.OsPath qualified as Directory
import System.OsPath (OsPath)


data FilePaths = FilePaths
  { home :: OsPath
  , xdgCache :: OsPath
  , xdgConfig :: OsPath
  , xdgData :: OsPath
  , xdgState :: OsPath
  }
  deriving (Show, Eq)


getFilePaths :: IO FilePaths
getFilePaths = do
  home <- Directory.getHomeDirectory

  xdgCache <- Directory.getXdgDirectory Directory.XdgCache mempty
  xdgConfig <- Directory.getXdgDirectory Directory.XdgConfig mempty
  xdgData <- Directory.getXdgDirectory Directory.XdgData mempty
  xdgState <- Directory.getXdgDirectory Directory.XdgState mempty

  pure FilePaths{..}
