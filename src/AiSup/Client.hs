module AiSup.Client where

import Control.Monad (when)
import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)

import qualified Data.ByteString as Bs
import qualified Data.ByteString.Char8 as B8
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Te
import Data.UUID (UUID)
import qualified Data.UUID as Uu

import System.IO (IOMode(WriteMode), withBinaryFile)

import GHC.Generics (Generic)

import qualified Data.Aeson as Ae
import qualified Data.Aeson.Types as Ae
import Data.Aeson ((.=), (.:))

import qualified Network.HTTP.Client as Hc
import qualified Network.HTTP.Client.TLS as Hct
import qualified Network.HTTP.Types.Status as Hs
import qualified Network.HTTP.Types.Header as Hh

import AiSup.Types


loginAiServer :: AiRunnerCfg -> IO AiClient
loginAiServer cfg = do
  manager <- Hc.newManager Hct.tlsManagerSettings
  req0 <- Hc.parseRequest (cfg.baseUrl <> "/login")
  let
    body = Ae.encode $ Ae.object [ "username" .= cfg.username, "secret" .= cfg.secret ]
    request = req0 {
            Hc.method = "POST", Hc.requestHeaders = [("Content-Type", "application/json")]
          , Hc.requestBody = Hc.RequestBodyLBS body
          }

  response <- Hc.httpLbs request manager
  ensureHttp2xx "@[loginAiServer]" response

  loginResult <- either (throwIO . userError . ("@[loginAiServer] Could not decode AI login response: " <>))
      pure
      (Ae.eitherDecode response.responseBody :: Either String AiLoginResult)

  case loginResult of
    AiAuthenticated sess -> pure AiClient {
          manager = manager
        , baseUrl = cfg.baseUrl
        , jwt = sess.jwt
        }
    AiUnauthorized msg ->
      throwIO . userError $ "@[loginAiServer] unauthorized: " <> T.unpack msg
    AiError msg ->
      throwIO . userError $ "@[loginAiServer] error: " <> T.unpack msg


invokeForAsset :: AiClient -> UUID -> Ae.Value -> Ae.Value -> IO (UUID, UUID)
invokeForAsset ai functionEid params content = do
  req0 <- Hc.parseRequest (ai.baseUrl <> "/invoke")
  let
    body = Ae.encode $ Ae.object [ 
              "function" .= functionEid
            , "context" .= Ae.Null
            , "parameters" .= params
            , "content" .= content
            , "files" .= ([] :: [UUID])
            , "references" .= ([] :: [UUID])
            ]
    req = req0 {
            Hc.method = "POST"
          , Hc.requestHeaders = bearerHeaders ai.jwt <> [("Content-Type", "application/json")]
          , Hc.requestBody = Hc.RequestBodyLBS body
          }

  resp <- Hc.httpLbs req ai.manager
  ensureHttp2xx "@[invokeForAsset]" resp

  inv <- either (throwIO . userError . ("@[invokeForAsset] invoke response decode err: " <>))
      pure
      (Ae.eitherDecode resp.responseBody :: Either String AiInvokeResponse)

  remoteAssetEid <- waitForRemoteAsset ai inv.requestEId
  pure (inv.requestEId, remoteAssetEid)


waitForRemoteAsset :: AiClient -> UUID -> IO UUID
waitForRemoteAsset ai requestEid =
  waitLoop (0 :: Int)
  where
  waitLoop n = do
    when (n > 600) $
      throwIO . userError $ "@[waitForRemoteAsset] request timed out, eid: " <> Uu.toString requestEid

    req0 <- Hc.parseRequest $ ai.baseUrl <> "/invoke/response?tid=" <> Uu.toString requestEid
    let req = req0 { Hc.requestHeaders = bearerHeaders ai.jwt }

    resp <- Hc.httpLbs req ai.manager
    ensureHttp2xx "@[waitForRemoteAsset]" resp

    inv <- either (throwIO . userError . ("@[waitForRemoteAsset] poll response decode err: " <>))
        pure
        (Ae.eitherDecode resp.responseBody :: Either String AiInvokeResponse)

    case decodeResponseState inv.result of
      AiNotReady -> do
        threadDelay 1000000
        waitLoop (n + 1)
      AiReady assetEid -> pure assetEid
      AiAborted msg -> throwIO . userError $ "@[waitForRemoteAsset] AI request aborted: " <> T.unpack msg
      AiOther val -> do
        -- putStrLn $ "@[waitForRemoteAsset] waiting 1 sec."
        threadDelay 1000000
        -- putStrLn $ "@[waitForRemoteAsset] 1 sec passed."
        if n > 10 then
          throwIO . userError $ "@[waitForRemoteAsset] unexpected AI response shape: " <> show val
        else
          waitLoop (n + 1)


decodeResponseState :: Ae.Value -> AiResponseState
decodeResponseState =
  fromMaybe (AiOther Ae.Null) . Ae.parseMaybe parser
  where
    parser = Ae.withObject "AiResponseState" $ \o -> do
      rk <- o .: "result"
      Ae.withObject "ResponseKind" (\x -> do
        tag <- x .: "tag"
        case (tag :: Text) of
          "NoResponseYetRK" -> pure AiNotReady
          "AssetRK" -> do
            c <- x .: "contents"
            eid <- c .: "assetEId"
            pure $ AiReady eid
          "AbortedRK" -> AiAborted <$> x .: "contents"
          _ -> pure $ AiOther rk
        ) rk


downloadRemoteAiAsset :: AiClient -> UUID -> FilePath -> IO ()
downloadRemoteAiAsset ai assetEid outPath = do
  req0 <- Hc.parseRequest $
    ai.baseUrl <> "/asset/" <> Uu.toString assetEid
  let req =
        req0
          { Hc.requestHeaders =
              [ ("Authorization", "Bearer " <> Te.encodeUtf8 ai.jwt)
              ]
          }

  Hc.withResponse req ai.manager $ \resp -> do
    ensureHttp2xx "AI asset download" resp
    withBinaryFile outPath WriteMode $ \h ->
      let loop = do
            chunk <- Hc.brRead resp.responseBody
            if Bs.null chunk
              then pure ()
              else Bs.hPut h chunk >> loop
      in loop



ensureHttp2xx :: String -> Hc.Response body -> IO ()
ensureHttp2xx label resp =
  let
    code = Hs.statusCode resp.responseStatus
  in do
  when (code < 200 || code >= 300) $
    throwIO . userError $ label <> " failed with HTTP "
          <> show code <> " " <> B8.unpack (Hs.statusMessage resp.responseStatus)

bearerHeaders :: Text -> [Hh.Header]
bearerHeaders jwt = [ ("Authorization", "Bearer " <> Te.encodeUtf8 jwt) ]

