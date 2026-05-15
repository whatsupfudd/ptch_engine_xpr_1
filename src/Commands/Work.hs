module Commands.Work (workCmd) where

import qualified Control.Monad.Cont as Mc
import Control.Monad (void)

import Data.Int (Int64)
import Data.Text (Text, unpack)
import Data.UUID (UUID, fromString)

import Hasql.Pool (Pool)

import Options.Runtime (RunOptions (..), PgDbConfig (..), AiConfig (..))
import Options.Cli (WorkOpts (..))
import DB.Connect (startPg)

import Pitcher.Render.WorkTypes ( WorkerCaps(..), LeasedNode(..) )
import Pitcher.Render.TaskRunner (VideoRenderCfg (..), TaskRunnerEnv (..), runLeasedNodeToCompletion)
import Pitcher.Render.WorkerMain (LeaseRecycleMode (..), WorkerLoopCfg (..), defaultWorkerLoopCfg, runWorkerLoop)
import Pitcher.Render.GraphTypes (NodeExec (..), textToNodeExec)
import Assets.Types (S3Conn (..))
import Assets.S3Ops (makeS3Conn)
import AiSup.Types (AiRunnerCfg (..))

workCmd :: WorkOpts -> RunOptions -> IO ()
workCmd opts rtOpts =
  case validateConf rtOpts.aiConf of
    Left errs -> error $ "@[workCmd] ai conf errs: " <> show errs
    Right (ttsSpeaker, ttsFunctionEid, imageFunctionEid) ->
      let
        pgPool = startPg rtOpts.pgDbConf
        wCap = WorkerCaps {
          owner = opts.owner
          , lane = opts.lane
          , leaseSeconds = opts.leaseSeconds
          , execFilter = Nothing
        }
        aiCfg = AiRunnerCfg {
            baseUrl = rtOpts.aiConf.server
          , username = rtOpts.aiConf.user
          , secret = rtOpts.aiConf.password
          , ttsVoice = Just ttsSpeaker
          , ttsFunctionEid = ttsFunctionEid
          , imageFunctionEid = imageFunctionEid
          , imageModel = rtOpts.aiConf.imageModel
          -- , imagePromptPrefix = "This is a storyboard sketch inspired by Aurélie Charbonnier that aims to build the key cinematographic and design details of the scene. The visuals for the scene are described as: "
          -- , imagePromptPostfix = " . The image is a portrait format, it is only the sketch and has no annotations or descriptions about the storyboard scene details, low resolution and uses a crayon drawing style."
          -- , imagePromptPrefix = "This is a photorealistic image that aims to build the key cinematographic and design details of the scene, in the context of a UAE-centered environment. The visuals for the scene are described as: "
          --, imagePromptPostfix = ". The image is a portrait format (9:16), it has no annotations or descriptions about the scene details, it is low resolution."
          }
        s3Cfg = makeS3Conn rtOpts.s3store
        videoCfg = VideoRenderCfg {
            ffmpegBin = "/opt/homebrew/bin/ffmpeg"
          , ffprobeBin = "/opt/homebrew/bin/ffprobe"
          , widthPx = 1920
          , heightPx = 1080
          , fps = 24
          , gapDurationSeconds = 0.5
          , fadeDurationSeconds = 0.5
          }
      in do
      -- putStrLn $ "@[workCmd] using s3store: " <> show rtOpts.s3store
      Mc.runContT pgPool (mainWorker wCap aiCfg s3Cfg videoCfg)


validateConf :: AiConfig -> Either [String] (Text, UUID, UUID)
validateConf aiConf =
  case aiConf.ttsFunctionEid of
    Nothing -> Left ["TTS function EID is required"]
    Just ttsFunctionEid ->
      case aiConf.imageFunctionEid of
        Nothing -> Left ["Image function EID is required"]
        Just imageFunctionEid ->
          Right (aiConf.ttsSpeaker, ttsFunctionEid, imageFunctionEid)

{-
WorkerMain:
-}
mainWorker :: WorkerCaps -> AiRunnerCfg -> S3Conn -> VideoRenderCfg -> Pool -> IO ()
mainWorker caps ai s3 video pgPool = do
  let
    runnerEnv = TaskRunnerEnv {
        pool = pgPool
        , s3 = s3
        , ai = ai
        , video = video
      }
    cfg =
        defaultWorkerLoopCfg
          { recycleMode = RecycleExpiredGlobally 20
          , logMsg = putStrLn . unpack
          }
  _stats <- runWorkerLoop runnerEnv caps cfg
  pure ()

{-
WorkerLease:
runOneWorker :: WorkerCaps -> Pool -> IO ()
runOneWorker caps pool = do
  mbNode <- leaseNextNode pool caps
  case mbNode of
    Nothing -> putStrLn "@[runOneWorker] No node to lease."
    Just node -> do
      putStrLn $ "@[runOneWorker] Leased node: " <> show node
      -- spawn a heartbeat thread in real code
      -- run task based on node.exec / node.payload
      result <- runNodeTask node
      case result of
        Right (assetUid, assetEid) ->
          void $ completeNodeSuccess pool CompleteSuccess {
                  nodeUid = node.nodeUid
                , owner = caps.owner
                , assetUid = assetUid
                , assetEid = assetEid
                , requestEid = Nothing
                , notes = Nothing
              }
        Left errTxt ->
          void $ completeNodeFailure pool CompleteFailure {
                  nodeUid = node.nodeUid
                , owner = caps.owner
                , retryable = True
                , errorText = errTxt
                , notes = Nothing
                , requestEid = Nothing
                }

-- Fake implementation for now:
runNodeTask :: LeasedNode -> IO (Either Text (Int64, UUID))
runNodeTask node =
  let
    mbExec = textToNodeExec node.exec
  in
  case mbExec of
    Nothing ->
      pure (Left "Unknown exec" :: Either Text (Int64, UUID))
    Just exec -> case exec of
      AiTextToSpeechExec -> do
        putStrLn "@[runNodeTask] AiTextToSpeechExec"
        pure . Left $ "@[runNodeTask] Unimplemented AiTextToSpeechExec"
      AiTextToImageExec -> do
        putStrLn "@[runNodeTask] AiTextToImageExec"
        pure . Left $ "@[runNodeTask] Unimplemented AiTextToImageExec"
      FfmpegSegmentExec -> do
        putStrLn "@[runNodeTask] FfmpegSegmentExec"
        pure . Left $ "@[runNodeTask] Unimplemented FfmpegSegmentExec"
      FfmpegConcatExec -> do
        putStrLn "@[runNodeTask] FfmpegConcatExec"
        pure . Left $ "@[runNodeTask] Unimplemented FfmpegConcatExec"
      BlenderExec -> do
        putStrLn "@[runNodeTask] BlenderExec"
        pure . Left $ "@[runNodeTask] Unimplemented BlenderExec"
      _ ->
        pure . Left $ "@[runNodeTask] Unknown exec: " <> node.exec
-}