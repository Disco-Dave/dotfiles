module Config where

import Config.Colors (Colors)
import Config.Font (Font)
import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)

data Config
  = Config
      { font :: Font,
        colors :: Colors,
        dynamicTitle :: Bool
      }
  deriving (Show, Generic)

instance Aeson.ToJSON Config where
  toJSON Config {..} =
    Aeson.object
      [ "font" Aeson..= font,
        "colors" Aeson..= colors,
        "dynamic_title" Aeson..= dynamicTitle
      ]
  toEncoding Config {..} =
    Aeson.pairs . mconcat $
      [ "font" Aeson..= font,
        "colors" Aeson..= colors,
        "dynamic_title" Aeson..= dynamicTitle
      ]
