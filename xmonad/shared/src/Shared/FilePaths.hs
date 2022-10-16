module Shared.FilePaths
  ( FilePaths (..)
  , getFilePaths
  )
where

import System.Directory qualified as Directory


data FilePaths = FilePaths
  { home :: FilePath
  , xdgCache :: FilePath
  , xdgConfig :: FilePath
  , xdgData :: FilePath
  }
  deriving (Show, Eq)


getFilePaths :: IO FilePaths
getFilePaths = do
  home <- Directory.getHomeDirectory

  xdgCache <- Directory.getXdgDirectory Directory.XdgCache mempty
  xdgConfig <- Directory.getXdgDirectory Directory.XdgConfig mempty
  xdgData <- Directory.getXdgDirectory Directory.XdgData mempty

  pure FilePaths{..}
