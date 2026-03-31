module Utils where

import qualified Data.ByteString.Lazy as Lbs
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Te
import qualified Data.Aeson as Ae

lastDef :: a -> [a] -> a
lastDef def xs =
  case reverse xs of
    [] -> def
    y : _ -> y

squashWs :: Text -> Text
squashWs = T.unwords . T.words


trim :: String -> String
trim = T.unpack . T.strip . T.pack


sanitizeKey :: Text -> Text
sanitizeKey = T.map (\c -> if isAsciiAlphaNum c then c else '_')


isAsciiAlphaNum :: Char -> Bool
isAsciiAlphaNum c = isAsciiLower c || isAsciiUpper c || isDigit c

tshow :: Show a => a -> Text
tshow = T.pack . show

sigText :: Ae.Value -> Text
sigText = Te.decodeUtf8 . Lbs.toStrict . Ae.encode


maybeToList :: Maybe a -> [a]
maybeToList = maybe [] pure

quote :: String -> String
quote s = "\"" ++ s ++ "\""
