module Shared.Hostname
  ( Hostname (..)
  , fromText
  , getHostname
  , toText
  , toString
  )
where

import Data.ByteString qualified as ByteString
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Encoding
import Data.Text.Encoding.Error (lenientDecode)
import System.Directory.OsPath (doesFileExist)
import System.OsPath (OsPath, osp)
import System.OsPath qualified as OsPath
import Prelude hiding (readFile)


data Hostname
  = Laptop
  | Desktop
  | Virt
  | Work
  | Other
  deriving (Show, Eq)


fromText :: Text -> Hostname
fromText text
  | is "desktop" = Desktop
  | is "laptop" = Laptop
  | is "virt" = Virt
  | is "PA-DBURKE1021" = Work
  | otherwise = Other
 where
  normalize = Text.toCaseFold . Text.strip
  normalizedText = normalize text
  is hostname = normalize hostname == normalizedText


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


readFile :: OsPath -> IO Text
readFile osPath = do
  fileExists <- doesFileExist osPath

  if fileExists
    then do
      filePath <- OsPath.decodeUtf osPath
      bytes <- ByteString.readFile filePath
      pure $ Encoding.decodeUtf8With lenientDecode bytes
    else pure mempty


getHostname :: IO Hostname
getHostname =
  fromText <$> readFile [osp|/etc/hostname|]
