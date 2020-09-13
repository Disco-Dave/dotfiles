module Config.Window
  ( Window (..),
  )
where

import qualified Data.Aeson as Aeson

newtype Window = Window
  { dynamicTitle :: Bool
  }
  deriving (Show, Eq)

instance Aeson.ToJSON Window where
  toJSON Window {..} =
    Aeson.object ["dynamic_title" Aeson..= dynamicTitle]
  toEncoding Window {..} =
    Aeson.pairs $ "dynamic_title" Aeson..= dynamicTitle
