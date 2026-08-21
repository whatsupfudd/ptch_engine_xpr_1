module Commands.Publish (publishCmd) where

import qualified Control.Monad.Cont as Mc
import Control.Exception (throwIO)

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.UUID as Uu

import Hasql.Pool (Pool)
import Hasql.Session (statement)

import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import Assets.S3Ops (downloadAssetToPath, makeS3Conn)
import Assets.Types (S3Conn)
import DB.Connect (startPg)
import DB.Helpers (runSessionOrThrow)
import qualified DB.PublishStmt as Pst
import qualified DB.ProducerStmt as Pbs
import Options.Cli (PublishOpts (..))
import Options.Runtime (RunOptions (..))
import Publish.YouTube (publishVideo)
import Publish.YouTube.Types
  ( YouTubeConfig (..)
  , YouTubeVideoSpec (..)
  , YouTubeUploadResult (..)
  )


publishCmd :: PublishOpts -> RunOptions -> IO ()
publishCmd opts rtOpts =
  case validateYouTubeConf rtOpts.youTubeConf of
    Left err -> throwIO . userError $ "@[publishCmd] " <> err
    Right () ->
      let
        pgPool = startPg rtOpts.pgDbConf
        s3Conn = makeS3Conn rtOpts.s3store
      in
      Mc.runContT pgPool $ publishProduction opts rtOpts.youTubeConf s3Conn


publishProduction :: PublishOpts -> YouTubeConfig -> S3Conn -> Pool -> IO ()
publishProduction opts youtubeConf s3Conn pool =
  case Uu.fromText opts.prodID of
    Nothing ->
      throwIO . userError $ "@[publishProduction] invalid production ID: " <> show opts.prodID
    Just prodEid -> do
      mbProdID <- runSessionOrThrow "selectProductionByIDStmt" pool $ statement prodEid Pbs.selectRenderJobByEid
      case mbProdID of
        Nothing ->
          throwIO . userError $ "@[publishProduction] production " <> show prodEid <> " not found."
        Just prodID -> do
          mbSource <- runSessionOrThrow "selectPublishSourceStmt" pool $ statement prodID Pst.selectPublishSourceStmt

          case mbSource of
            Nothing -> throwIO . userError $ "@[publishProduction] production " <> show prodEid 
                  <> " is not completed or has no final asset."

            Just (eidAsset, titleDefault) ->
              let
                title = fromMaybe titleDefault opts.titleVideo
                spec = YouTubeVideoSpec {
                      titleVideo = title
                    , descriptionVideo = opts.descriptionVideo
                    , tagsVideo = opts.tagsVideo
                    , idCategory = opts.categoryID
                    , privacyVideo = opts.privacyVideo
                    , notifySubscribers = opts.notifySubscribers
                    , madeForKids = opts.madeForKids
                    , syntheticMedia = opts.syntheticMedia
                    }
              in do
              validateVideoSpec spec

              withSystemTempDirectory "narravid-youtube" $ \tempDir -> do
                let pathVideo = tempDir </> "final.mp4"

                putStrLn $ "Retrieving final asset for production " <> show prodEid <> "..."
                downloadAssetToPath s3Conn eidAsset pathVideo
                putStrLn $ "Publishing production " <> show prodEid <> " to YouTube..."

                result <- publishVideo youtubeConf spec pathVideo

                putStrLn $ "Published production " <> show prodEid <> "; YouTube video ID: "
                    <> T.unpack result.idVideo


validateYouTubeConf :: YouTubeConfig -> Either String ()
validateYouTubeConf conf
  | T.null conf.idClient = Left "youtube.clientId is required."
  | otherwise = Right ()


validateVideoSpec :: YouTubeVideoSpec -> IO ()
validateVideoSpec spec
  | T.null . T.strip $ spec.titleVideo = throwIO . userError $ "@[publishProduction] YouTube title is empty."
  | spec.privacyVideo `notElem` ["private", "unlisted", "public"] =
      throwIO . userError $ "@[publishProduction] invalid privacy: " <> T.unpack spec.privacyVideo
  | otherwise = pure ()