module XMonad.Local.Environment (
  Environment (..),
  getEnvironment,
) where

import XMonad.Local.Hostname (Hostname)
import qualified XMonad.Local.Hostname as Hostname
import XMonad.Local.Theme (Theme)
import qualified XMonad.Local.Theme as Theme

data Environment = Environment
  { envHostname :: Hostname
  , envTheme :: Theme
  }
  deriving (Show, Eq)

getEnvironment :: IO Environment
getEnvironment = do
  hostname <- Hostname.getHostname
  pure $
    Environment
      { envHostname = hostname
      , envTheme = Theme.nord
      }
