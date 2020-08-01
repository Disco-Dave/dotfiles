module Config where

import Config.Window (Window)
import Config.Colors (Colors)
import Config.Font (Font)
import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)

data Config
  = Config
      { font :: Font,
        colors :: Colors,
        window :: Window
      }
  deriving (Show, Generic)

instance Aeson.ToJSON Config where
  toJSON Config {..} =
    Aeson.object
      [ "font" Aeson..= font,
        "colors" Aeson..= colors,
        "window" Aeson..= window
      ]
  toEncoding Config {..} =
    Aeson.pairs . mconcat $
      [ "font" Aeson..= font,
        "colors" Aeson..= colors,
        "window" Aeson..= window
      ]
