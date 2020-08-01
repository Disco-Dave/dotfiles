module Config.Window where

import GHC.Generics (Generic)
import qualified Data.Aeson as Aeson

newtype Window = Window 
  { windowDynamicTitle :: Bool
  } deriving (Show, Eq, Generic)


instance Aeson.ToJSON Window where
  toJSON Window{..} = 
    Aeson.object [ "dynamic_title" Aeson..= windowDynamicTitle ]
  toEncoding Window{..} = 
    Aeson.pairs $ mconcat [ "dynamic_title" Aeson..= windowDynamicTitle ]
