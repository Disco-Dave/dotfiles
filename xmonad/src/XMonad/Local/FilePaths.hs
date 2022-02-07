module XMonad.Local.FilePaths (
  FilePaths (..),
  getFilePaths,
) where

import qualified System.Directory as Directory

data FilePaths = FilePaths
  { home :: FilePath
  , xdgCache :: FilePath
  , xdgConfig :: FilePath
  , xdgData :: FilePath
  , xdgState :: FilePath
  }
  deriving (Show, Eq)

getFilePaths :: IO FilePaths
getFilePaths = do
  FilePaths
    <$> Directory.getHomeDirectory
    <*> Directory.getXdgDirectory Directory.XdgCache ""
    <*> Directory.getXdgDirectory Directory.XdgConfig ""
    <*> Directory.getXdgDirectory Directory.XdgData ""
    <*> Directory.getXdgDirectory Directory.XdgState ""
