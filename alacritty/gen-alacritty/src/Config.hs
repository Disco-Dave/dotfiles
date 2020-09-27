module Config
  ( Config (..),
  )
where

import AesonOptions (options)
import Config.Colors (Colors)
import Config.Font (Font)
import Config.Window (Window)
import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)

data Config = Config
  { font :: !Font,
    colors :: !Colors,
    window :: !Window
  }
  deriving (Show, Generic)

instance Aeson.ToJSON Config where
  toJSON = Aeson.genericToJSON options
  toEncoding = Aeson.genericToEncoding options
