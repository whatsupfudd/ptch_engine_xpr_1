module Publish.YouTube.OAuth
  ( YouTubeAuthResult (..)
  , authorizeYouTube
  , refreshAccessToken
  , resolveRefreshToken
  , writeRefreshToken
  , youtubeUploadScope
  )
where

import Control.Concurrent.MVar
  ( MVar
  , newEmptyMVar
  , takeMVar
  , tryPutMVar
  )
import qualified Control.Exception as Cexc
import Control.Monad (join, unless, void, when)

import Crypto.Hash (Digest, SHA256, hash)
import Crypto.Random (getRandomBytes)

import qualified Data.Aeson as Ae
import Data.Aeson ((.:), (.:?), (.!=))
import Data.ByteArray (ByteArrayAccess)
import Data.ByteArray.Encoding
  ( Base (Base64URLUnpadded)
  , convertToBase
  )
import qualified Data.ByteString as Bs
import qualified Data.ByteString.Char8 as Bsc
import qualified Data.ByteString.Lazy as Lbs
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Te
import qualified Data.Text.IO as Tio

import Network.HTTP.Client
  ( Manager
  , Request (..)
  , Response
  , httpLbs
  , newManager
  , parseRequest
  , responseBody
  , responseStatus
  , urlEncodedBody
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types
  ( status200
  , status404
  , statusCode
  )
import Network.HTTP.Types.URI (renderQuery)
import Network.Wai
  ( Application
  , Request
  , queryString
  , rawPathInfo
  , responseLBS
  )
import qualified Network.Wai as Nw
import Network.Wai.Handler.Warp (withApplication)

import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.Info (os)
import System.Posix.Files (setFileMode)
import System.Process (callProcess)
import System.Timeout (timeout)

import Publish.YouTube.Types (YouTubeConfig (..))


youtubeUploadScope :: Text
youtubeUploadScope = "https://www.googleapis.com/auth/youtube.upload"


data YouTubeAuthResult = YouTubeAuthResult
  { tokenAccess :: Text
  , tokenRefreshResult :: Text
  , scopesGranted :: [Text]
  }
  deriving (Eq, Show)


data OAuthCallback = OAuthCallback
  { codeCallback :: Maybe Text
  , stateCallback :: Maybe Text
  , errorCallback :: Maybe Text
  , errorDescriptionCallback :: Maybe Text
  }


data OAuthTokenResponse = OAuthTokenResponse
  { accessTokenResponse :: Text
  , refreshTokenResponse :: Maybe Text
  , scopeResponse :: Text
  }
  deriving (Eq, Show)


instance Ae.FromJSON OAuthTokenResponse where
  parseJSON =
    Ae.withObject "OAuthTokenResponse" $ \obj ->
      OAuthTokenResponse
        <$> obj .: "access_token"
        <*> obj .:? "refresh_token"
        <*> obj .:? "scope" .!= ""


authorizeYouTube :: YouTubeConfig  -> Bool -> Int -> Maybe Text -> IO YouTubeAuthResult
authorizeYouTube conf openBrowser timeoutSeconds loginHint = do
  validateAuthorizeConf conf

  verifier <- randomBase64Url 64
  state <- randomBase64Url 32
  callbackVar <- newEmptyMVar
  manager <- newManager tlsManagerSettings

  withApplication (pure $ callbackApp callbackVar) $ \port -> do
    let
      redirectUri = T.pack $ "http://localhost:" <> show port <> "/oauth2callback"
      url = authorizationUrl conf.idClient redirectUri verifier state loginHint

    putStrLn "Authorize Narravid to upload videos to YouTube."
    putStrLn ""
    putStrLn "Open this URL in your browser:"
    putStrLn ""
    Tio.putStrLn url
    putStrLn ""

    when openBrowser $ do
      opened <- openSystemBrowser url
      unless opened $ putStrLn "Could not launch a browser automatically; open the URL above."

    mbCallback <- timeout (timeoutSeconds * 1000000) (takeMVar callbackVar)

    callback <-
      case mbCallback of
        Nothing -> Cexc.throwIO . userError $ "@[authorizeYouTube] timed out waiting for OAuth callback."
        Just value -> pure value

    code <- validateCallback state callback
    tokenResponse <- exchangeAuthorizationCode manager conf redirectUri verifier code
    let scopes = T.words tokenResponse.scopeResponse
    unless (youtubeUploadScope `elem` scopes) $
      Cexc.throwIO . userError $ "@[authorizeYouTube] YouTube upload scope was not granted."

    putStrLn $ "tokenResponse: " <> show tokenResponse
    refreshToken <- case tokenResponse.refreshTokenResponse of
      Nothing -> Cexc.throwIO . userError $ "@[authorizeYouTube] Google did not return a refresh token."
      Just value -> pure value

    pure $
      YouTubeAuthResult
        { tokenAccess = tokenResponse.accessTokenResponse
        , tokenRefreshResult = refreshToken
        , scopesGranted = scopes
        }


authorizationUrl
  :: Text
  -> Text
  -> Text
  -> Text
  -> Maybe Text
  -> Text
authorizationUrl clientId redirectUri verifier state loginHint =
  Te.decodeUtf8 $
    "https://accounts.google.com/o/oauth2/v2/auth"
      <> renderQuery True params
  where
  params =
    [ ("client_id", Just $ Te.encodeUtf8 clientId)
    , ("redirect_uri", Just $ Te.encodeUtf8 redirectUri)
    , ("response_type", Just "code")
    , ("scope", Just $ Te.encodeUtf8 youtubeUploadScope)
    , ("code_challenge", Just $ Te.encodeUtf8 $ pkceChallenge verifier)
    , ("code_challenge_method", Just "S256")
    , ("state", Just $ Te.encodeUtf8 state)
    , ("prompt", Just "consent")
    ]
    <> catMaybes
      [ fmap
          (\value -> ("login_hint", Just $ Te.encodeUtf8 value))
          loginHint
      ]


exchangeAuthorizationCode
  :: Manager
  -> YouTubeConfig
  -> Text
  -> Text
  -> Text
  -> IO OAuthTokenResponse
exchangeAuthorizationCode manager conf redirectUri verifier code = do
  req0 <- parseRequest "https://oauth2.googleapis.com/token"

  let
    fields =
      [ ("client_id", Te.encodeUtf8 conf.idClient)
      , ("code", Te.encodeUtf8 code)
      , ("code_verifier", Te.encodeUtf8 verifier)
      , ("grant_type", "authorization_code")
      , ("redirect_uri", Te.encodeUtf8 redirectUri)
      ]
      <> maybe []
        (\secret -> [("client_secret", Te.encodeUtf8 secret)])
        conf.secretClient

    req =
      noStatusException $
        urlEncodedBody fields req0

  resp <- httpLbs req manager

  if statusCode (responseStatus resp) /= 200
    then throwHttpError "exchangeAuthorizationCode" resp
    else
      case Ae.eitherDecode (responseBody resp) of
        Left err ->
          Cexc.throwIO . userError $
            "@[exchangeAuthorizationCode] invalid response: " <> err

        Right result ->
          pure result


refreshAccessToken :: YouTubeConfig -> IO Text
refreshAccessToken conf = do
  refreshToken <- resolveRefreshToken conf
  manager <- newManager tlsManagerSettings
  req0 <- parseRequest "https://oauth2.googleapis.com/token"

  let
    fields =
      [ ("client_id", Te.encodeUtf8 conf.idClient)
      , ("refresh_token", Te.encodeUtf8 refreshToken)
      , ("grant_type", "refresh_token")
      ]
      <> maybe []
        (\secret -> [("client_secret", Te.encodeUtf8 secret)])
        conf.secretClient

    req =
      noStatusException $
        urlEncodedBody fields req0

  resp <- httpLbs req manager

  if statusCode (responseStatus resp) /= 200 then
    throwHttpError "refreshAccessToken" resp
  else
    case Ae.eitherDecode (responseBody resp) :: Either String OAuthTokenResponse of
      Left err -> Cexc.throwIO . userError $ "@[refreshAccessToken] invalid response: " <> err
      Right token -> pure token.accessTokenResponse


resolveRefreshToken :: YouTubeConfig -> IO Text
resolveRefreshToken conf =
  case conf.pathTokenRefresh of
    Just path -> do
      token <- T.strip <$> Tio.readFile path

      if T.null token
        then
          Cexc.throwIO . userError $
            "@[resolveRefreshToken] refresh-token file is empty: " <> path
        else
          pure token

    Nothing ->
      case conf.tokenRefresh of
        Just token
          | not . T.null . T.strip $ token ->
              pure $ T.strip token

        _ ->
          Cexc.throwIO . userError $
            "@[resolveRefreshToken] no YouTube refresh token configured."


writeRefreshToken :: FilePath -> Text -> IO ()
writeRefreshToken path token = do
  createDirectoryIfMissing True $ takeDirectory path
  Tio.writeFile path $ T.strip token <> "\n"

  -- Narravid currently targets Unix-like deployment environments.
  -- Keep the long-lived OAuth credential owner-readable/writable only.
  setFileMode path 0o600


validateAuthorizeConf :: YouTubeConfig -> IO ()
validateAuthorizeConf conf
  | T.null . T.strip $ conf.idClient =
      Cexc.throwIO . userError $
        "@[authorizeYouTube] youtube.clientId is required."
  | otherwise =
      pure ()


validateCallback :: Text -> OAuthCallback -> IO Text
validateCallback expectedState callback =
  case callback.errorCallback of
    Just err ->
      Cexc.throwIO . userError $
        "@[authorizeYouTube] OAuth authorization failed: "
          <> T.unpack err
          <> maybe ""
            (\desc -> " (" <> T.unpack desc <> ")")
            callback.errorDescriptionCallback

    Nothing
      | callback.stateCallback /= Just expectedState ->
          Cexc.throwIO . userError $
            "@[authorizeYouTube] OAuth state mismatch."

      | otherwise ->
          case callback.codeCallback of
            Nothing ->
              Cexc.throwIO . userError $
                "@[authorizeYouTube] OAuth callback contained no code."

            Just code ->
              pure code


callbackApp :: MVar OAuthCallback -> Application
callbackApp callbackVar req respond
  | rawPathInfo req /= "/oauth2callback" = respond $ responseLBS status404
          [("Content-Type", "text/plain; charset=utf-8")]
          "Not found."

  | otherwise = do
      let
        callback =
          OAuthCallback
            { codeCallback = queryParam "code" req
            , stateCallback = queryParam "state" req
            , errorCallback = queryParam "error" req
            , errorDescriptionCallback =
                queryParam "error_description" req
            }

      void $ tryPutMVar callbackVar callback

      respond $
        responseLBS
          status200
          [ ("Content-Type", "text/html; charset=utf-8")
          , ("Cache-Control", "no-store")
          ]
          callbackHtml


queryParam :: Bs.ByteString -> Nw.Request -> Maybe Text
queryParam name req = Te.decodeUtf8 <$> join (lookup name $ Nw.queryString req)


callbackHtml :: Lbs.ByteString
callbackHtml =
  "<!doctype html>\
  \<html>\
  \<head><meta charset=\"utf-8\"><title>Narravid YouTube authorization</title></head>\
  \<body style=\"font-family:sans-serif;max-width:42rem;margin:4rem auto\">\
  \<h1>Narravid</h1>\
  \<p>The YouTube authorization response has been received.</p>\
  \<p>You can close this window and return to the terminal.</p>\
  \</body>\
  \</html>"


randomBase64Url :: Int -> IO Text
randomBase64Url size = do
  bytes <- getRandomBytes size :: IO Bs.ByteString
  pure $ encodeBase64Url bytes


pkceChallenge :: Text -> Text
pkceChallenge verifier =
  let
    digest =
      hash (Te.encodeUtf8 verifier) :: Digest SHA256
  in
  encodeBase64Url digest


encodeBase64Url :: ByteArrayAccess bytes => bytes -> Text
encodeBase64Url =
  Te.decodeUtf8 . convertToBase Base64URLUnpadded


openSystemBrowser :: Text -> IO Bool
openSystemBrowser url =
  case browserCommand of
    Nothing ->
      pure False

    Just (command, args) -> do
      result <-
        Cexc.try (callProcess command args)
          :: IO (Either Cexc.SomeException ())

      pure $
        case result of
          Left _ -> False
          Right () -> True
  where
  urlS = T.unpack url

  browserCommand =
    case os of
      "darwin" -> Just ("open", [urlS])
      "linux" -> Just ("xdg-open", [urlS])
      _ -> Nothing


noStatusException :: Network.HTTP.Client.Request -> Network.HTTP.Client.Request
noStatusException req = req { checkResponse = \_ _ -> pure () }


throwHttpError :: String -> Response Lbs.ByteString -> IO a
throwHttpError label resp =
  let
    code = statusCode $ responseStatus resp
    body = Bsc.unpack . Lbs.toStrict . Lbs.take 4096 $ responseBody resp
  in
  Cexc.throwIO . userError $ "@[" <> label <> "] HTTP " <> show code <> ": " <> body