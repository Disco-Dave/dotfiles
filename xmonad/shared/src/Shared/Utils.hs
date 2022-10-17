module Shared.Utils
  ( readFileToText
  , showToText
  , normalizeText
  , enumFromText
  )
where

import qualified Data.ByteString as ByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import Data.Text.Encoding.Error (lenientDecode)
import System.Directory (doesFileExist)


showToText :: Show a => a -> Text
showToText =
  Text.pack . show


readFileToText :: FilePath -> IO (Maybe Text)
readFileToText filePath = do
  fileExists <- doesFileExist filePath

  if fileExists
    then do
      bytes <- ByteString.readFile filePath
      pure . Just $ Encoding.decodeUtf8With lenientDecode bytes
    else pure Nothing


normalizeText :: Text -> Text
normalizeText =
  Text.toCaseFold . Text.strip


enumFromText :: forall a. (Bounded a, Enum a) => (a -> Text) -> Text -> Maybe a
enumFromText toText rawText =
  lookup (normalizeText rawText) $
    [ (normalizeText $ toText option, option)
    | option <- [minBound @a ..]
    ]
