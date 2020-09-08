module Config.Window where

import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)

newtype Window = Window
  { dynamicTitle :: Bool
  }
  deriving (Show, Eq, Generic)

instance Aeson.ToJSON Window
