module Commands.Launch (launchCmd) where

import qualified Control.Monad.Cont as Mc

import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID, fromString)

import Hasql.Pool (Pool)

import DB.Connect (startPg)
import Pitcher.Render.Launch (RenderEnv (..), RenderParallelism (..), launchRender)
import Options.Cli (LaunchOpts (..))
import Options.Runtime (RunOptions (..), PgDbConfig (..), AiConfig (..))
import Pitcher.Render.Types (RenderOutcome (..))
import AiSup.Types (AiRunnerCfg (..))
import Assets.S3Ops (makeS3Conn)

launchCmd :: LaunchOpts -> RunOptions -> IO ()
launchCmd lOpts rtOpts = do
  putStrLn $ "@[launchCmd] rtOpts: " <> show rtOpts
  case fromString lOpts.jobUid of
    Nothing -> error $ "@[launchCmd] invalid job UID: " <> show lOpts.jobUid
    Just jobEid ->
      case validateConf rtOpts.aiConf of
        Left errs -> error $ "@[launchCmd] ai conf errs: " <> show errs
        Right (ttsSpeaker, ttsFunctionEid, imageFunctionEid) ->
          let
            aiCfg = AiRunnerCfg {
              baseUrl = rtOpts.aiConf.server
              , username = rtOpts.aiConf.user
              , secret = rtOpts.aiConf.password
              , ttsFunctionEid = ttsFunctionEid
              , ttsVoice = Just ttsSpeaker
              , imageFunctionEid = imageFunctionEid
              , imageModel = rtOpts.aiConf.imageModel
              -- , imagePromptPrefix = "This is a storyboard sketch inspired by Aurélie Charbonnier that aims to build the key cinematographic and design details of the scene. The visuals for the scene are described as: "
              -- , imagePromptPostfix = " . The image is a portrait format, it is only the sketch and has no annotations or descriptions about the storyboard scene details, low resolution and uses a crayon drawing style."
            }
            env = RenderEnv {
                aiCfg = aiCfg
              , s3Conn = makeS3Conn rtOpts.s3store
              , ffmpegBin = "/opt/homebrew/bin/ffmpeg"
              , ffprobeBin = "/opt/homebrew/bin/ffprobe"
              , widthPx = 1920
              , heightPx = 1080
              , fps = 24
              , gapDurationSeconds = 0.5
              , fadeDurationSeconds = 0.5
              , renderVersionTag = "v1"
              , failFast = False
              , parallelism = RenderParallelism {
                    audioWorkers = 1
                    , imageWorkers = 1
                    , segmentWorkers = 1
                  }
            }
          in
          let
            pgPool = startPg rtOpts.pgDbConf
          in do
          Mc.runContT pgPool (launchJob env jobEid)

validateConf :: AiConfig -> Either [String] (Text, UUID, UUID)
validateConf aiConf =
      case aiConf.ttsFunctionEid of
        Nothing -> Left ["TTS function EID is required"]
        Just ttsFunctionEid ->
          case aiConf.imageFunctionEid of
            Nothing -> Left ["Image function EID is required"]
            Just imageFunctionEid ->
              Right (aiConf.ttsSpeaker, ttsFunctionEid, imageFunctionEid)


launchJob :: RenderEnv -> UUID -> Pool -> IO ()
launchJob env narrationEid pool = do
    putStrLn $ "Launching job " <> show narrationEid
    rez <- launchRender env narrationEid pool
    case rez of
      RenderSucceeded { finalAssetEid = eid } -> do
        putStrLn $ "Job " <> show narrationEid <> " completed successfully."
        putStrLn $ "Final asset EID: " <> show eid
      RenderFailed { reason = msg } -> do
        putStrLn $ "Job " <> show narrationEid <> " failed: " <> T.unpack msg
