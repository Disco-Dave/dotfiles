module XMonad.Local.Hostname (
  Hostname (..),
  fromText,
  getHostname,
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
  | Other Text
  deriving (Show, Eq)

fromText :: Text -> Hostname
fromText (Text.strip . Text.toCaseFold -> rawHostName)
  | is "desktop" = Desktop
  | is "laptop" = Laptop
  | is "virt" = Virt
  | otherwise = Other rawHostName
 where
  is hostname = Text.toCaseFold hostname == rawHostName

getHostname :: IO Hostname
getHostname = do
  bytes <- ByteString.readFile "/etc/hostname"
  let rawHostname = Encoding.decodeUtf8With lenientDecode bytes
  pure $ fromText rawHostname

