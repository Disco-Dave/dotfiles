module Shared.Hostname
  ( Hostname (..)
  , fromText
  , getHostname
  , toText
  , toString
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Shared.Utils (enumFromText, readFileToText)
import System.OsPath (osp)
import Prelude hiding (readFile)


data Hostname
  = Laptop
  | Desktop
  | Virt
  | Work
  | Other
  deriving (Show, Eq, Bounded, Enum)


toText :: Hostname -> Text
toText = \case
  Laptop -> "laptop"
  Desktop -> "desktop"
  Virt -> "virt"
  Work -> "PA-DBURKE1021"
  Other -> "other"


fromText :: Text -> Hostname
fromText =
  fromMaybe Other . enumFromText toText


toString :: Hostname -> String
toString =
  Text.unpack . toText


getHostname :: IO Hostname
getHostname = do
  contents <- fromMaybe mempty <$> readFileToText [osp|/etc/hostname|]
  pure $ fromText contents
