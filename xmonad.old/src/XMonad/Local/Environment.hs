module XMonad.Local.Environment (
  Environment (..),
  getEnvironment,
) where

import XMonad.Local.FilePaths (FilePaths)
import qualified XMonad.Local.FilePaths as FilePaths
import XMonad.Local.Hostname (Hostname)
import qualified XMonad.Local.Hostname as Hostname
import XMonad.Local.Theme (Theme)
import qualified XMonad.Local.Theme as Theme

data Environment = Environment
  { envTheme :: Theme
  , envHostname :: Hostname
  , envFilePaths :: FilePaths
  }
  deriving (Show, Eq)

getEnvironment :: IO Environment
getEnvironment =
  Environment Theme.nord
    <$> Hostname.getHostname
    <*> FilePaths.getFilePaths
