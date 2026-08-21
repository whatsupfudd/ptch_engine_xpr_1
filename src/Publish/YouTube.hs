module Publish.YouTube (publishVideo)
where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, throwIO, try)

import qualified Data.ByteString as Bs
import qualified Data.ByteString.Char8 as Bsc
import qualified Data.ByteString.Lazy as Lbs
import qualified Data.CaseInsensitive as CI
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Te
import qualified Data.Text.IO as Tio

import qualified Data.Aeson as Ae
import Data.Aeson ((.:), (.=))

import Network.HTTP.Client
  ( Manager
  , Request (..)
  , RequestBody (..)
  , Response
  , httpLbs
  , newManager
  , parseRequest
  , responseBody
  , responseHeaders
  , responseStatus
  , responseTimeoutNone
  , urlEncodedBody
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)

import System.Directory (getFileSize)
import System.IO
  ( IOMode (ReadMode)
  , SeekMode (AbsoluteSeek)
  , hSeek
  , withBinaryFile
  )
import Text.Read (readMaybe)

import Publish.YouTube.Types
import qualified Publish.YouTube.OAuth as YtO


newtype TokenResponse = TokenResponse
  { accessToken :: Text
  }


instance Ae.FromJSON TokenResponse where
  parseJSON =
    Ae.withObject "TokenResponse" $ \obj ->
      TokenResponse <$> obj .: "access_token"


data UploadResponse = UploadResponse
  { uploadVideoId :: Text
  }


instance Ae.FromJSON UploadResponse where
  parseJSON =
    Ae.withObject "UploadResponse" $ \obj ->
      UploadResponse <$> obj .: "id"


publishVideo :: YouTubeConfig -> YouTubeVideoSpec -> FilePath -> IO YouTubeUploadResult
publishVideo conf spec path = do
  manager <- newManager tlsManagerSettings
  token <- refreshAccessToken manager conf
  sizeInteger <- getFileSize path
  let size = fromIntegral sizeInteger :: Int64

  if size <= 0 then
    throwIO . userError $ "@[publishVideo] refusing to upload an empty video."
  else do
    uploadUrl <- startUpload manager token spec size
    upload <- uploadResumable manager token uploadUrl path size
    pure $ YouTubeUploadResult upload.uploadVideoId


refreshAccessToken :: Manager -> YouTubeConfig -> IO Text
refreshAccessToken manager conf = do
  req0 <- parseRequest "https://oauth2.googleapis.com/token"
  mbTokenRefresh <- case conf.pathTokenRefresh of
    Just path -> do
      content <- Tio.readFile path
      pure $ Just content
    Nothing -> pure conf.tokenRefresh
  let
    req = noStatusException $ urlEncodedBody
          [ ("client_id", Te.encodeUtf8 conf.idClient)
          , ("client_secret", Te.encodeUtf8 $ fromMaybe "" conf.secretClient)
          , ("refresh_token", Te.encodeUtf8 $ fromMaybe "" mbTokenRefresh)
          , ("grant_type", "refresh_token")
          ]
          req0

  -- putStrLn $ "req: " <> show req

  resp <- httpLbs req manager
  -- putStrLn $ "resp: " <> show resp
  if statusCode (responseStatus resp) /= 200 then
    throwHttpError "refreshAccessToken" resp
  else
    case Ae.eitherDecode (responseBody resp) :: Either String TokenResponse of
      Left err -> throwIO . userError $ "@[refreshAccessToken] invalid token response: " <> err
      Right token -> pure token.accessToken


startUpload :: Manager -> Text -> YouTubeVideoSpec -> Int64 -> IO String
startUpload manager token spec size = do
  let
    notifyTxt = if spec.notifySubscribers then "true" else "false"
    endpoint = "https://www.googleapis.com/upload/youtube/v3/videos" <> "?uploadType=resumable"
        <> "&part=snippet%2Cstatus" <> "&notifySubscribers=" <> notifyTxt
    metadata = Ae.encode $ Ae.object [ "snippet" .= Ae.object snippetFields , "status" .= Ae.object statusFields ]
    snippetFields =
      [ "title" .= spec.titleVideo, "description" .= spec.descriptionVideo, "categoryId" .= spec.idCategory ]
      <> if null spec.tagsVideo then [] else ["tags" .= spec.tagsVideo]
    statusFields = [ "privacyStatus" .= spec.privacyVideo ]
      <> maybe [] (\value -> ["selfDeclaredMadeForKids" .= value]) spec.madeForKids
      <> maybe [] (\value -> ["containsSyntheticMedia" .= value]) spec.syntheticMedia

  req0 <- parseRequest endpoint
  let
    req = noStatusException $ req0 { 
            method = "POST"
          , requestHeaders = requestHeaders req0 <> [
                bearerHeader token
              , ("Content-Type", "application/json; charset=UTF-8")
              , ("X-Upload-Content-Length", Te.encodeUtf8 . T.pack $ show size)
              , ("X-Upload-Content-Type", "video/mp4")
              ]
          , requestBody = RequestBodyLBS metadata
          }

  resp <- httpLbs req manager

  if statusCode (responseStatus resp) /= 200 then 
    throwHttpError "startUpload" resp
  else
    case lookup "Location" (responseHeaders resp) of
      Nothing -> throwIO . userError $ "@[startUpload] YouTube did not return an upload Location."
      Just location -> pure $ Bsc.unpack location


uploadResumable
  :: Manager
  -> Text
  -> String
  -> FilePath
  -> Int64
  -> IO UploadResponse
uploadResumable manager token uploadUrl path total =
  go 0 0
  where
  go offset retryCount = do
    eiResp <-
      try (sendBytes manager token uploadUrl path total offset)
        :: IO (Either SomeException (Response Lbs.ByteString))

    case eiResp of
      Left err ->
        recover offset retryCount $
          "@[uploadResumable] transport error: " <> show err

      Right resp ->
        case statusCode (responseStatus resp) of
          200 -> decodeUploadResponse resp
          201 -> decodeUploadResponse resp

          308 ->
            case nextUploadOffset resp of
              Left err ->
                throwIO . userError $ "@[uploadResumable] " <> err
              Right next ->
                go next 0

          status
            | status `elem` [500, 502, 503, 504] ->
                recover offset retryCount $
                  "@[uploadResumable] transient HTTP status "
                    <> show status

          _ ->
            throwHttpError "uploadResumable" resp

  recover oldOffset retryCount err
    | retryCount >= 5 =
        throwIO . userError $
          err <> "; retry limit reached."
    | otherwise = do
        threadDelay $ retryDelay retryCount
        state <- queryUploadState manager token uploadUrl total
        case state of
          UploadComplete result ->
            pure result
          UploadIncomplete next ->
            go next (retryCount + 1)


data UploadState
  = UploadComplete UploadResponse
  | UploadIncomplete Int64


queryUploadState
  :: Manager
  -> Text
  -> String
  -> Int64
  -> IO UploadState
queryUploadState manager token uploadUrl total = do
  req0 <- parseRequest uploadUrl

  let
    req =
      noStatusException $
        req0
          { method = "PUT"
          , requestHeaders =
              [ bearerHeader token
              , ("Content-Length", "0")
              , ("Content-Range", Bsc.pack $ "bytes */" <> show total)
              ]
          , requestBody = RequestBodyBS Bs.empty
          , responseTimeout = responseTimeoutNone
          }

  resp <- httpLbs req manager

  case statusCode (responseStatus resp) of
    200 -> UploadComplete <$> decodeUploadResponse resp
    201 -> UploadComplete <$> decodeUploadResponse resp
    308 ->
      case nextUploadOffset resp of
        Left err -> throwIO . userError $ "@[queryUploadState] " <> err
        Right next -> pure $ UploadIncomplete next
    _ -> throwHttpError "queryUploadState" resp


sendBytes
  :: Manager
  -> Text
  -> String
  -> FilePath
  -> Int64
  -> Int64
  -> IO (Response Lbs.ByteString)
sendBytes manager token uploadUrl path total offset = do
  req0 <- parseRequest uploadUrl

  let
    remaining = total - offset
    range =
      "bytes " <> show offset <> "-"
        <> show (total - 1) <> "/" <> show total

    req =
      noStatusException $
        req0
          { method = "PUT"
          , requestHeaders =
              [ bearerHeader token
              , ("Content-Type", "video/mp4")
              , ("Content-Range", Bsc.pack range)
              ]
          , requestBody = fileRequestBody path offset remaining
          , responseTimeout = responseTimeoutNone
          }

  httpLbs req manager


fileRequestBody :: FilePath -> Int64 -> Int64 -> RequestBody
fileRequestBody path offset size =
  RequestBodyStream size $ \givePopper ->
    withBinaryFile path ReadMode $ \handle -> do
      hSeek handle AbsoluteSeek $ fromIntegral offset
      givePopper $ Bs.hGetSome handle (1024 * 1024)


nextUploadOffset :: Response body -> Either String Int64
nextUploadOffset resp =
  case lookup "Range" (responseHeaders resp) of
    Nothing ->
      Right 0

    Just range ->
      case reverse $ Bsc.split '-' range of
        [] ->
          Left $ "invalid Range response: " <> Bsc.unpack range

        lastByte:_ ->
          case readMaybe (Bsc.unpack lastByte) of
            Nothing ->
              Left $ "invalid Range response: " <> Bsc.unpack range

            Just byte ->
              Right $ byte + 1


decodeUploadResponse :: Response Lbs.ByteString -> IO UploadResponse
decodeUploadResponse resp =
  case Ae.eitherDecode (responseBody resp) of
    Left err ->
      throwIO . userError $
        "@[decodeUploadResponse] invalid YouTube response: " <> err
    Right result ->
      pure result


bearerHeader :: Text -> (CI.CI Bs.ByteString, Bs.ByteString)
bearerHeader token = (CI.mk "Authorization", "Bearer " <> Te.encodeUtf8 token)


noStatusException :: Request -> Request
noStatusException req =
  req { checkResponse = \_ _ -> pure () }


retryDelay :: Int -> Int
retryDelay retryCount =
  1000000 * (2 ^ retryCount)


throwHttpError :: String -> Response Lbs.ByteString -> IO a
throwHttpError label resp =
  let
    status = statusCode $ responseStatus resp
    body = Bsc.unpack . Lbs.toStrict . Lbs.take 4096 $ responseBody resp
  in
  throwIO . userError $
    "@[" <> label <> "] HTTP " <> show status <> ": " <> body