module XMonad.Local.Hostname (
  Hostname (..),
  fromText,
  getHostname,
  toText,
  toString,
) where

import qualified Data.ByteString as ByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import Data.Text.Encoding.Error (lenientDecode)

data Hostname
  = Laptop
  | Desktop
  | Virt
  | Work
  | Other
  deriving (Show, Eq)

fromText :: Text -> Hostname
fromText (Text.strip . Text.toCaseFold -> rawHostName)
  | is "desktop" = Desktop
  | is "laptop" = Laptop
  | is "virt" = Virt
  | is "PA-DBURKE1021" = Work
  | otherwise = Other
 where
  is hostname = Text.toCaseFold hostname == rawHostName

toText :: Hostname -> Text
toText = \case
  Laptop -> "laptop"
  Desktop -> "desktop"
  Virt -> "virt"
  Work -> "PA-DBURKE1021"
  Other -> "other"

toString :: Hostname -> String
toString =
  Text.unpack . toText

getHostname :: IO Hostname
getHostname = do
  bytes <- ByteString.readFile "/etc/hostname"
  let rawHostname = Encoding.decodeUtf8With lenientDecode bytes
  pure $ fromText rawHostname
