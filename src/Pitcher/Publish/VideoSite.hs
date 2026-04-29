{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Pitcher.Publish.VideoSite
  ( HostingService(..)
  , YoutubeServiceCfg(..)
  , UserAccount(..)
  , YoutubeAccountAuth(..)
  , PublishMeta(..)
  , YoutubePrivacy(..)
  , PublishResult(..)
  , S3Def(..)
  , AssetRef(..)
  , publishToVideoSite
  ) where

import Control.Exception (throwIO)
import Control.Monad (when)

import Data.Bifunctor (first)
import qualified Data.ByteString as Bs
import qualified Data.ByteString.Char8 as B8
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Te
import Data.UUID (UUID)
import qualified Data.UUID as Uu

import System.Directory (getFileSize)
import System.FilePath ((</>))
import System.IO (IOMode(ReadMode), withBinaryFile)
import System.IO.Temp (withSystemTempDirectory)

import GHC.Generics (Generic)

import qualified Data.Aeson as Ae
import Data.Aeson ((.=))

import qualified Network.HTTP.Client as Hc
import qualified Network.HTTP.Client.TLS as Hct
import qualified Network.HTTP.Types.Header as Hh
import Network.HTTP.Types.URI (renderSimpleQuery)
import qualified Network.Minio as Mn
import qualified Network.HTTP.Types.Status as Hc

import Hasql.Pool (Pool)
import qualified Hasql.Pool as Hp
import Hasql.Session (Session, statement)
import Hasql.Statement (Statement)
import qualified Hasql.TH as TH

import DB.Helpers (runSessionOrThrow)
--------------------------------------------------------------------------------
-- Public types

newtype HostingService =  YoutubeHS YoutubeServiceCfg
  deriving (Eq, Show)

data YoutubeServiceCfg = YoutubeServiceCfg { 
    categoryId :: Text
  , defaultPrivacy :: YoutubePrivacy
  , notifySubscribers :: Bool
  }
  deriving (Eq, Show)

data YoutubePrivacy =
    YtPrivate
  | YtUnlisted
  | YtPublic
  deriving (Eq, Show)

data UserAccount = UserAccount
  { uid :: Int64
  , label :: Text
  , youtubeAuth :: Maybe YoutubeAccountAuth
  }
  deriving (Eq, Show)

data YoutubeAccountAuth = YoutubeAccountAuth
  { clientId :: Text
  , clientSecret :: Maybe Text
  , refreshToken :: Text
  , channelId :: Maybe Text
  }
  deriving (Eq, Show)

data PublishMeta = PublishMeta
  { title :: Maybe Text
  , description :: Maybe Text
  , tags :: [Text]
  , privacy :: Maybe YoutubePrivacy
  }
  deriving (Eq, Show)

data PublishResult = PublishResult
  { service :: Text
  , remoteId :: Text
  , remoteUrl :: Text
  , titleUsed :: Text
  , descriptionUsed :: Text
  , tagsUsed :: [Text]
  }
  deriving (Eq, Show, Generic)

data S3Def = S3Def
  { bucket :: Text
  , connInfo :: Mn.ConnectInfo
  }

data AssetRef = AssetRef
  { uid :: Int64
  , eid :: UUID
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Internal types

data AssetPublishDefaults = AssetPublishDefaults
  { assetName :: Maybe Text
  , contentType :: Maybe Text
  , narrationTitle :: Maybe Text
  , narrationNotes :: Maybe Text
  }
  deriving (Eq, Show)

data ResolvedPublishMeta = ResolvedPublishMeta
  { title :: Text
  , description :: Text
  , tags :: [Text]
  , privacy :: YoutubePrivacy
  }
  deriving (Eq, Show)

data GoogleTokenResponse = GoogleTokenResponse
  { access_token :: Text
  , token_type :: Maybe Text
  , expires_in :: Maybe Int
  , scope :: Maybe Text
  }
  deriving (Eq, Show, Generic, Ae.FromJSON)

data YoutubeVideoInsertResponse = YoutubeVideoInsertResponse
  { id :: Text
  }
  deriving (Eq, Show, Generic, Ae.FromJSON)

--------------------------------------------------------------------------------
-- Entry point

publishToVideoSite
  :: HostingService
  -> UserAccount
  -> Pool
  -> S3Def
  -> AssetRef
  -> Maybe PublishMeta
  -> IO PublishResult
publishToVideoSite hostingService userAccount pool s3Def assetRef mbMeta =
  case hostingService of
    YoutubeHS youtubeCfg ->
      publishToYoutube youtubeCfg userAccount pool s3Def assetRef mbMeta

--------------------------------------------------------------------------------
-- YouTube implementation

publishToYoutube
  :: YoutubeServiceCfg
  -> UserAccount
  -> Pool
  -> S3Def
  -> AssetRef
  -> Maybe PublishMeta
  -> IO PublishResult
publishToYoutube youtubeCfg userAccount pool s3Def assetRef mbMeta = do
  ytAuth <-
    case userAccount.youtubeAuth of
      Nothing ->
        throwIO . userError $
          "publishToYoutube: UserAccount does not contain YouTube OAuth credentials."
      Just auth ->
        pure auth

  defaults <- loadAssetPublishDefaults pool assetRef
  resolvedMeta <- pure $ resolvePublishMeta youtubeCfg defaults mbMeta
  mimeType <- pure $ resolveMimeType defaults.contentType

  withSystemTempDirectory "pitcher-youtube-publish" $ \tmpDir -> do
    let videoPath = tmpDir </> "video.mp4"
    downloadAssetToPath s3Def assetRef.eid videoPath

    accessToken <- refreshGoogleAccessToken ytAuth
    uploadUrl <-
      initiateYoutubeResumableUpload
        accessToken
        youtubeCfg
        resolvedMeta
        mimeType
        videoPath

    uploadResp <-
      uploadYoutubeMedia
        accessToken
        uploadUrl
        mimeType
        videoPath

    pure PublishResult
      { service = "youtube"
      , remoteId = uploadResp.id
      , remoteUrl = "https://www.youtube.com/watch?v=" <> uploadResp.id
      , titleUsed = resolvedMeta.title
      , descriptionUsed = resolvedMeta.description
      , tagsUsed = resolvedMeta.tags
      }

--------------------------------------------------------------------------------
-- Metadata resolution

resolvePublishMeta
  :: YoutubeServiceCfg
  -> AssetPublishDefaults
  -> Maybe PublishMeta
  -> ResolvedPublishMeta
resolvePublishMeta youtubeCfg defaults mbMeta =
  let override = fromMaybe emptyPublishMeta mbMeta
      titleTxt =
        firstNonEmpty
          [ override.title
          , defaults.narrationTitle
          , defaults.assetName
          , Just "Pitcher video"
          ]

      descTxt =
        firstNonEmpty
          [ override.description
          , defaults.narrationNotes
          , Just ""
          ]

      tagsTxt =
        normalizeTags override.tags

      privacyVal =
        fromMaybe youtubeCfg.defaultPrivacy override.privacy
  in ResolvedPublishMeta
      { title = titleTxt
      , description = descTxt
      , tags = tagsTxt
      , privacy = privacyVal
      }

emptyPublishMeta :: PublishMeta
emptyPublishMeta =
  PublishMeta
    { title = Nothing
    , description = Nothing
    , tags = []
    , privacy = Nothing
    }

firstNonEmpty :: [Maybe Text] -> Text
firstNonEmpty =
  go
  where
    go [] = ""
    go (Nothing : xs) = go xs
    go (Just t : xs)
      | T.null (T.strip t) = go xs
      | otherwise = T.strip t

normalizeTags :: [Text] -> [Text]
normalizeTags =
  dedupPreserve
  . filter (not . T.null)
  . map (T.strip . squashWs)

squashWs :: Text -> Text
squashWs =
  T.unwords . T.words

dedupPreserve :: Ord a => [a] -> [a]
dedupPreserve =
  go []
  where
    go _ [] = []
    go seen (x:xs)
      | x `elem` seen = go seen xs
      | otherwise = x : go (x : seen) xs

resolveMimeType :: Maybe Text -> Text
resolveMimeType mbMime =
  case fmap T.toLower mbMime of
    Just ct | "video/" `T.isPrefixOf` ct -> ct
    _ -> "video/mp4"

privacyText :: YoutubePrivacy -> Text
privacyText = \case
  YtPrivate -> "private"
  YtUnlisted -> "unlisted"
  YtPublic -> "public"

--------------------------------------------------------------------------------
-- OAuth2 token refresh

refreshGoogleAccessToken :: YoutubeAccountAuth -> IO Text
refreshGoogleAccessToken ytAuth = do
  manager <- Hc.newManager Hct.tlsManagerSettings
  req0 <- Hc.parseRequest "https://oauth2.googleapis.com/token"

  let
    formParams =
        [ ("client_id", ytAuth.clientId)
        , ("refresh_token", ytAuth.refreshToken)
        , ("grant_type", "refresh_token")
        ] <> maybe [] (\s -> [("client_secret", s)]) ytAuth.clientSecret

    req = req0 {
            Hc.method = "POST"
          , Hc.requestHeaders = [ ("Content-Type", "application/x-www-form-urlencoded") ]
          , Hc.requestBody = Hc.RequestBodyBS $ renderSimpleQuery False
                  [ (Te.encodeUtf8 k, Te.encodeUtf8 v)
                  | (k, v) <- formParams
                  ]
      }

  resp <- Hc.httpLbs req manager
  ensureHttp2xx "Google OAuth token refresh" resp

  tokenResp <- either (throwIO . userError . ("Could not decode Google token response: " <>))
      pure (Ae.eitherDecode resp.responseBody :: Either String GoogleTokenResponse)

  pure tokenResp.access_token


--------------------------------------------------------------------------------
-- YouTube resumable upload

initiateYoutubeResumableUpload
  :: Text
  -> YoutubeServiceCfg
  -> ResolvedPublishMeta
  -> Text
  -> FilePath
  -> IO String
initiateYoutubeResumableUpload accessToken youtubeCfg meta mimeType videoPath = do
  manager <- Hc.newManager Hct.tlsManagerSettings
  fileSize <- getFileSize videoPath

  req0 <-
    Hc.parseRequest $ "https://www.googleapis.com/upload/youtube/v3/videos"
        <> "?uploadType=resumable"
        <> "&part=snippet,status"
        <> "&notifySubscribers=" <> boolText youtubeCfg.notifySubscribers

  let body =
        Ae.object $
          [ "snippet" .= snippetObject youtubeCfg meta
          , "status" .= statusObject meta
          ]

      req =
        req0
          { Hc.method = "POST"
          , Hc.requestHeaders =
              [ ("Authorization", "Bearer " <> Te.encodeUtf8 accessToken)
              , ("Content-Type", "application/json; charset=UTF-8")
              , ("X-Upload-Content-Type", Te.encodeUtf8 mimeType)
              , ("X-Upload-Content-Length", B8.pack (show fileSize))
              ]
          , Hc.requestBody = Hc.RequestBodyLBS (Ae.encode body)
          }

  resp <- Hc.httpLbs req manager
  ensureHttp2xx "YouTube resumable-upload initiation" resp

  case lookup Hh.hLocation resp.responseHeaders of
    Nothing ->
      throwIO . userError $
        "YouTube resumable-upload initiation did not return a Location header."
    Just loc ->
      pure (B8.unpack loc)

snippetObject :: YoutubeServiceCfg -> ResolvedPublishMeta -> Ae.Value
snippetObject youtubeCfg meta =
  let baseFields =
        [ "title" .= meta.title
        , "description" .= meta.description
        , "categoryId" .= youtubeCfg.categoryId
        ]
      tagFields =
        if null meta.tags
          then []
          else ["tags" .= meta.tags]
  in Ae.object (baseFields <> tagFields)

statusObject :: ResolvedPublishMeta -> Ae.Value
statusObject meta =
  Ae.object
    [ "privacyStatus" .= privacyText meta.privacy
    ]

uploadYoutubeMedia
  :: Text
  -> String
  -> Text
  -> FilePath
  -> IO YoutubeVideoInsertResponse
uploadYoutubeMedia accessToken uploadUrl mimeType videoPath = do
  manager <- Hc.newManager Hct.tlsManagerSettings
  fileSize <- fromIntegral <$> getFileSize videoPath
  req0 <- Hc.parseRequest uploadUrl

  let req =
        req0
          { Hc.method = "PUT"
          , Hc.requestHeaders =
              [ ("Authorization", "Bearer " <> Te.encodeUtf8 accessToken)
              , ("Content-Type", Te.encodeUtf8 mimeType)
              , ("Content-Length", B8.pack (show fileSize))
              ]
          , Hc.requestBody = mkFileRequestBody videoPath fileSize
          }

  resp <- Hc.httpLbs req manager
  ensureHttp2xx "YouTube media upload" resp

  either
    (throwIO . userError . ("Could not decode YouTube upload response: " <>))
    pure
    (Ae.eitherDecode resp.responseBody :: Either String YoutubeVideoInsertResponse)

mkFileRequestBody :: FilePath -> Int64 -> Hc.RequestBody
mkFileRequestBody path size =
  Hc.RequestBodyStream size $ \usePopper ->
    withBinaryFile path ReadMode $ \h ->
      usePopper $ Bs.hGetSome h (1024 * 1024)

boolText :: Bool -> String
boolText True = "true"
boolText False = "false"

ensureHttp2xx :: String -> Hc.Response body -> IO ()
ensureHttp2xx label resp = do
  let sc = resp.responseStatus
      code = Hc.statusCode sc
  when (code < 200 || code >= 300) $
    throwIO . userError $
      label
        <> " failed with HTTP "
        <> show code
        <> " "
        <> B8.unpack (Hc.statusMessage sc)

--------------------------------------------------------------------------------
-- S3 fetch

downloadAssetToPath :: S3Def -> UUID -> FilePath -> IO ()
downloadAssetToPath s3Def assetEid outPath = do
  rez <- Mn.runMinio s3Def.connInfo $
    Mn.fGetObject
      s3Def.bucket
      (Uu.toText assetEid)
      outPath
      Mn.defaultGetObjectOptions

  case rez of
    Left err ->
      throwIO . userError $
        "S3 download failed for asset "
          <> Uu.toString assetEid
          <> ": "
          <> show err
    Right _ ->
      pure ()

--------------------------------------------------------------------------------
-- DB metadata lookup

loadAssetPublishDefaults :: Pool -> AssetRef -> IO AssetPublishDefaults
loadAssetPublishDefaults pool assetRef =
  runSessionOrThrow "selectAssetPublishDefaultsStmt" pool $
    statement assetRef.eid selectAssetPublishDefaultsStmt >>= \case
      Nothing ->
        pure AssetPublishDefaults
          { assetName = Nothing
          , contentType = Nothing
          , narrationTitle = Nothing
          , narrationNotes = Nothing
          }
      Just (assetName, contentType, narrationTitle, narrationNotes) ->
        pure AssetPublishDefaults
          { assetName = assetName
          , contentType = contentType
          , narrationTitle = narrationTitle
          , narrationNotes = narrationNotes
          }

-- This query reuses the schema introduced earlier in the project:
--   asset
--   prod.render_job(final_asset_fk, narration_fk)
--   prod.narration(title, notes)
selectAssetPublishDefaultsStmt
  :: Statement UUID (Maybe (Maybe Text, Maybe Text, Maybe Text, Maybe Text))
selectAssetPublishDefaultsStmt =
  [TH.maybeStatement|
    select
      a.name::text?,
      a.contentType::text?,
      k.title::text?,
      k.notes::text?
    from asset a
    left join prod.render_job r
      on r.final_asset_fk = a.uid
    left join prod.narration k
      on k.uid = r.narration_fk
    where a.eid = $1::uuid
    order by r.uid desc nulls last
    limit 1
  |]