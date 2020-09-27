module Config.Window
  ( Window (..),
  )
where

import AesonOptions (options)
import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)

newtype Window = Window
  { dynamicTitle :: Bool
  }
  deriving (Show, Eq, Generic)

instance Aeson.ToJSON Window where
  toJSON = Aeson.genericToJSON options
  toEncoding = Aeson.genericToEncoding options
