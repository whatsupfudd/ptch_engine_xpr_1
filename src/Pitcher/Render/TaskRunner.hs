{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Pitcher.Render.TaskRunner
  ( TaskRunnerEnv(..)
  , VideoRenderCfg(..)
  , NodeComputeSuccess(..)
  , NodeComputeFailure(..)
  , runLeasedNodeToCompletion
  , dispatchNodeCompute
  ) where

import Control.Concurrent (forkIO, killThread, threadDelay, ThreadId)
import Control.Concurrent.MVar
  ( MVar
  , newEmptyMVar
  , putMVar
  , tryReadMVar
  )
import Control.Exception
  ( SomeException
  , bracket
  , throwIO
  , try
  )
import Control.Monad (forM, forM_, unless, void, when)

import Data.Bifunctor (first)
import qualified Data.ByteString as Bs
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as Lbs
import Data.Int (Int64, Int32)
import qualified Data.List as L
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Te
import Data.UUID (UUID)
import qualified Data.UUID as Uu
import qualified Data.UUID.V4 as Uu4
import qualified Data.Vector as Vc

import System.Directory (getFileSize)
import System.FilePath ((</>))
import System.IO (IOMode(WriteMode), withBinaryFile)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

import GHC.Generics (Generic)

import qualified Data.Aeson as Ae
import qualified Data.Aeson.Types as Ae
import Data.Aeson ((.:), (.=))

import Hasql.Pool (Pool)
import qualified Hasql.Pool as Hp
import Hasql.Session (Session, statement)
import Hasql.Statement (Statement)
import qualified Hasql.TH as TH

import qualified Network.HTTP.Client as Hc
import qualified Network.HTTP.Client.TLS as Hct
import qualified Network.HTTP.Types.Status as Hs
-- import Network.HTTP.Types.URI (urlEncodeVars)

import qualified Network.Minio as Mn

import DB.Helpers (runSessionOrThrow, runTx)
import Assets.Types (S3Conn, AssetRef (..))
import qualified AiSup.Types as Ai
import Pitcher.Render.WorkerLease
  ( CompleteFailure(..)
  , CompleteSuccess(..)
  --, ExecRequirements(..)
  , LeasedNode(..)
  , WorkerCaps(..)
  , completeNodeFailure
  , completeNodeSuccess
  , heartbeatNodeLease
  )

import Pitcher.Render.WorkTypes (assetNameForNode)
import AiSup.Client (loginAiServer, invokeForAsset, downloadRemoteAiAsset)
import qualified Assets.Store as As
import qualified Assets.S3Ops as Ao
import qualified DB.TaskStmt as Ts
import Utils (lastDef, squashWs, trim, quote)

--------------------------------------------------------------------------------
-- Environment

data TaskRunnerEnv = TaskRunnerEnv {
    pool :: Pool
  , s3 :: S3Conn
  , ai :: Ai.AiRunnerCfg
  , video :: VideoRenderCfg
  }


data VideoRenderCfg = VideoRenderCfg {
    ffmpegBin :: FilePath
  , ffprobeBin :: FilePath
  , widthPx :: Int
  , heightPx :: Int
  , fps :: Int
  , gapDurationSeconds :: Double
  , fadeDurationSeconds :: Double
  }
  deriving (Show)


--------------------------------------------------------------------------------
-- Dispatch result

data NodeComputeSuccess = NodeComputeSuccess
  { asset :: AssetRef
  , requestEid :: Maybe UUID
  , notes :: Maybe Text
  }
  deriving (Eq, Show)

data NodeComputeFailure = NodeComputeFailure
  { retryable :: Bool
  , errorText :: Text
  , notes :: Maybe Text
  , requestEid :: Maybe UUID
  }
  deriving (Eq, Show)

fatalFailure :: Text -> NodeComputeFailure
fatalFailure msg =
  NodeComputeFailure
    { retryable = False
    , errorText = msg
    , notes = Nothing
    , requestEid = Nothing
    }

retryableFailure :: Text -> NodeComputeFailure
retryableFailure msg =
  NodeComputeFailure
    { retryable = True
    , errorText = msg
    , notes = Nothing
    , requestEid = Nothing
    }

--------------------------------------------------------------------------------
-- Payloads

data AudioPayload = AudioPayload
  { task :: Text
  , dialogueUid :: Int64
  , emotion :: Text
  , spokenText :: Text
  }
  deriving (Eq, Show, Generic, Ae.FromJSON)

data ImagePayload = ImagePayload
  { task :: Text
  , dialogueUid :: Int64
  , visualOrd :: Int
  , sentenceIx :: Maybe Int
  , description :: Text
  }
  deriving (Eq, Show, Generic, Ae.FromJSON)

data SegmentPayload = SegmentPayload
  { task :: Text
  , dialogueUid :: Int64
  , audioNodeKey :: Text
  , imageNodeKeys :: [Text]
  , spokenText :: Text
  }
  deriving (Eq, Show, Generic, Ae.FromJSON)

data FinalPayload = FinalPayload
  { task :: Text
  , segmentNodeKeys :: [Text]
  , gapSeconds :: Maybe Double
  , fadeSeconds :: Maybe Double
  }
  deriving (Eq, Show, Generic, Ae.FromJSON)

--------------------------------------------------------------------------------
-- Public entry points

runLeasedNodeToCompletion :: TaskRunnerEnv -> WorkerCaps -> LeasedNode -> IO Bool
runLeasedNodeToCompletion env caps node = do
  putStrLn $ "runLeasedNodeToCompletion: " <> show node.key
  withHeartbeat env.pool caps node $ do
    res <- try (dispatchNodeCompute env node) :: IO (Either SomeException (Either NodeComputeFailure NodeComputeSuccess))
    putStrLn $ "runLeasedNodeToCompletion: res: " <> show res
    case res of
      Left ex -> do
        putStrLn $ "runLeasedNodeToCompletion: exception: " <> show ex
        completeNodeFailure
          env.pool
          CompleteFailure
            { nodeUid = node.nodeUid
            , owner = caps.owner
            , retryable = True
            , errorText = T.pack (show ex)
            , notes = Nothing
            , requestEid = Nothing
            }
      Right (Left failure) -> do
        putStrLn $ "runLeasedNodeToCompletion: failure: " <> show failure
        completeNodeFailure
          env.pool
          CompleteFailure
            { nodeUid = node.nodeUid
            , owner = caps.owner
            , retryable = failure.retryable
            , errorText = failure.errorText
            , notes = failure.notes
            , requestEid = failure.requestEid
            }
      Right (Right success) -> do
        putStrLn $ "runLeasedNodeToCompletion: success: " <> show success
        completeNodeSuccess
          env.pool
          CompleteSuccess
            { nodeUid = node.nodeUid
            , owner = caps.owner
            , assetUid = success.asset.uid
            , assetEid = success.asset.eid
            , requestEid = success.requestEid
            , notes = success.notes
            }

dispatchNodeCompute :: TaskRunnerEnv -> LeasedNode -> IO (Either NodeComputeFailure NodeComputeSuccess)
dispatchNodeCompute env node =
  case node.exec of
    "ai_tts" -> runAudioNode env node
    "ai_image" -> runImageNode env node
    "ffmpeg_segment" -> runSegmentNode env node
    "ffmpeg_concat" -> runFinalNode env node
    "blender" ->
      pure . Left $ fatalFailure "Blender execution is not implemented in this first-phase runner."
    other ->
      pure . Left . fatalFailure $ "Unsupported node.exec value: " <> other

--------------------------------------------------------------------------------
-- Heartbeat wrapper

withHeartbeat :: Pool -> WorkerCaps -> LeasedNode -> IO a -> IO a
withHeartbeat pool caps node action =
  bracket start stop (const action)
  where
  periodMicros = max 1000000 ((max 3 caps.leaseSeconds `div` 3) * 1000000)

  start :: IO (MVar (), ThreadId)
  start = do
    stopVar <- newEmptyMVar
    tid <- forkIO $ loop stopVar
    pure (stopVar, tid)

  stop :: (MVar (), ThreadId) -> IO ()
  stop (stopVar, tid) = do
    putMVar stopVar ()
    killThread tid

  loop :: MVar () -> IO ()
  loop stopVar = do
    threadDelay $ fromIntegral periodMicros
    mbStop <- tryReadMVar stopVar
    case mbStop of
      Just _ -> pure ()
      Nothing ->
        heartbeatNodeLease pool node.nodeUid caps >> loop stopVar

--------------------------------------------------------------------------------
-- Audio node

runAudioNode :: TaskRunnerEnv -> LeasedNode -> IO (Either NodeComputeFailure NodeComputeSuccess)
runAudioNode env node =
  case parsePayload "AudioPayload" node.payload :: Either Text AudioPayload of
    Left err -> do
      putStrLn $ "@[runAudioNode] err: " <> show err
      pure . Left $ fatalFailure err
    Right payload -> do
      aiClient <- loginAiServer env.ai
      let
        params = maybe Ae.Null (\voice -> Ae.object ["voice" .= voice]) env.ai.ttsVoice

      invokeRes <-
        try $ invokeForAsset aiClient env.ai.ttsFunctionEid params (Ae.toJSON payload.spokenText)
          :: IO (Either SomeException (UUID, UUID))

      case invokeRes of
        Left ex -> pure . Left . retryableFailure $ "@[runAudioNode] TTS invoke failed: " <> T.pack (show ex)
        Right (reqEid, remoteAssetEid) ->
          withSystemTempDirectory "pitcher-run-audio" $ \tmpDir -> do
            let
              localPath = tmpDir </> "audio.mp3"
            putStrLn $ "@[runAudioNode] will put audio at: " <> localPath
            dlRes <- try $ downloadRemoteAiAsset aiClient remoteAssetEid localPath :: IO (Either SomeException ())
            case dlRes of
              Left ex -> do
                putStrLn $ "@[runAudioNode] TTS asset download failed: " <> show remoteAssetEid
                pure . Left . retryableFailure $ "TTS asset download failed: " <> T.pack (show ex)
              Right () -> do
                putStrLn $ "@[runAudioNode] will upload audio to asset store"
                upRes <- try $ As.uploadFileAsAsset env.pool env.s3 localPath
                        (assetNameForNode node "mp3") "audio/mpeg" ("audio node " <> node.key) :: IO (Either SomeException AssetRef)
                case upRes of
                  Left ex -> do
                    putStrLn $ "@[runAudioNode] TTS asset upload failed: " <> show ex
                    pure . Left . retryableFailure $ "@[runAudioNode] TTS asset upload failed: " <> T.pack (show ex)
                  Right assetRef -> do
                    putStrLn $ "@[runAudioNode] TTS asset uploaded successfully" <> show assetRef
                    pure . Right $
                      NodeComputeSuccess
                        { asset = assetRef
                        , requestEid = Just reqEid
                        , notes = Just ("remoteAsset=" <> Uu.toText remoteAssetEid)
                        }

--------------------------------------------------------------------------------
-- Image node

runImageNode :: TaskRunnerEnv -> LeasedNode -> IO (Either NodeComputeFailure NodeComputeSuccess)
runImageNode env node =
  case parsePayload "ImagePayload" node.payload :: Either Text ImagePayload of
    Left err ->
      pure . Left $ fatalFailure err
    Right payload -> do
      aiClient <- loginAiServer env.ai
      let
        fullPrompt = env.ai.imagePromptPrefix <> payload.description <> env.ai.imagePromptPostfix
        params = Ae.object ["model" .= env.ai.imageModel]

      invokeRes <-
        try $ invokeForAsset aiClient env.ai.imageFunctionEid params (Ae.toJSON fullPrompt)
          :: IO (Either SomeException (UUID, UUID))

      case invokeRes of
        Left ex ->
          pure . Left . retryableFailure $
            "Image invoke failed: " <> T.pack (show ex)
        Right (reqEid, remoteAssetEid) ->
          withSystemTempDirectory "pitcher-run-image" $ \tmpDir -> do
            let localPath = tmpDir </> "image.png"
            dlRes <- try $ downloadRemoteAiAsset aiClient remoteAssetEid localPath
              :: IO (Either SomeException ())
            case dlRes of
              Left ex ->
                pure . Left . retryableFailure $
                  "Image asset download failed: " <> T.pack (show ex)
              Right () -> do
                upRes <- try $
                  As.uploadFileAsAsset
                    env.pool
                    env.s3
                    localPath
                    (assetNameForNode node "png")
                    "image/png"
                    ("image node " <> node.key)
                  :: IO (Either SomeException AssetRef)
                case upRes of
                  Left ex ->
                    pure . Left . retryableFailure $
                      "Image asset upload failed: " <> T.pack (show ex)
                  Right assetRef ->
                    pure . Right $
                      NodeComputeSuccess
                        { asset = assetRef
                        , requestEid = Just reqEid
                        , notes = Just ("remoteAsset=" <> Uu.toText remoteAssetEid)
                        }

--------------------------------------------------------------------------------
-- Segment node

runSegmentNode :: TaskRunnerEnv -> LeasedNode -> IO (Either NodeComputeFailure NodeComputeSuccess)
runSegmentNode env node =
  case parsePayload "SegmentPayload" node.payload :: Either Text SegmentPayload of
    Left err ->
      pure . Left $ fatalFailure err
    Right payload -> do
      audioInput <- lookupInputAssetByNodeKey env.pool node.graphUid payload.audioNodeKey
      imageInputs <- mapM (lookupInputAssetByNodeKey env.pool node.graphUid) payload.imageNodeKeys

      case (audioInput, sequence imageInputs) of
        (Nothing, _) ->
          pure . Left $
            fatalFailure $
              "Segment node is missing upstream audio asset for key " <> payload.audioNodeKey
        (_, Nothing) ->
          pure . Left $
            fatalFailure "Segment node is missing one or more upstream image assets."
        (Just audioAsset, Just imageAssets) ->
          withSystemTempDirectory "pitcher-run-segment" $ \tmpDir -> do
            let audioPath = tmpDir </> "dialogue.mp3"
                outPath = tmpDir </> "segment.mp4"

            Ao.downloadAssetToPath env.s3 audioAsset.eid audioPath

            imagePaths <- forM (zip [(1 :: Int) ..] imageAssets) $ \(ix, assetRef) -> do
              let path = tmpDir </> ("img_" <> show ix <> ".png")
              Ao.downloadAssetToPath env.s3 assetRef.eid path
              pure path

            durationRes <- try $ probeDurationSeconds env.video.ffprobeBin audioPath
              :: IO (Either SomeException Double)
            case durationRes of
              Left ex ->
                pure . Left . retryableFailure $
                  "ffprobe failed for segment audio: " <> T.pack (show ex)
              Right audioDuration -> do
                sentenceBodies <- loadDialogueSentences env.pool payload.dialogueUid
                visualAnchors <- loadDialogueVisualSentenceAnchors env.pool payload.dialogueUid

                let
                  shotPlan = buildTimedShotPlan sentenceBodies visualAnchors imagePaths audioDuration
                renderRes <-
                  try $
                    case imagePaths of
                      [] ->
                        renderAudioOnlySegment env.video audioPath audioDuration outPath
                      _ -> do
                        stillClips <- forM (zip [(1 :: Int) ..] shotPlan) $ \(ix, (imgPath, dur)) -> do
                          let clipPath = tmpDir </> ("still_" <> show ix <> ".mp4")
                          createStillClip env.video imgPath dur clipPath
                          pure clipPath
                        concatStillClipsWithAudio env.video stillClips audioPath outPath
                    :: IO (Either SomeException ())

                case renderRes of
                  Left ex ->
                    pure . Left . retryableFailure $
                      "ffmpeg segment render failed: " <> T.pack (show ex)
                  Right () -> do
                    upRes <- try $
                      As.uploadFileAsAsset
                        env.pool
                        env.s3
                        outPath
                        (assetNameForNode node "mp4")
                        "video/mp4"
                        ("segment node " <> node.key)
                      :: IO (Either SomeException AssetRef)

                    case upRes of
                      Left ex ->
                        pure . Left . retryableFailure $
                          "Segment upload failed: " <> T.pack (show ex)
                      Right assetRef ->
                        pure . Right $
                          NodeComputeSuccess
                            { asset = assetRef
                            , requestEid = Nothing
                            , notes = Nothing
                            }

--------------------------------------------------------------------------------
-- Final concat node

runFinalNode :: TaskRunnerEnv -> LeasedNode -> IO (Either NodeComputeFailure NodeComputeSuccess)
runFinalNode env node =
  case parsePayload "FinalPayload" node.payload :: Either Text FinalPayload of
    Left err ->
      pure . Left $ fatalFailure err
    Right payload -> do
      segInputs <- mapM (lookupInputAssetByNodeKey env.pool node.graphUid) payload.segmentNodeKeys
      case sequence segInputs of
        Nothing ->
          pure . Left $
            fatalFailure "Final node is missing one or more upstream segment assets."
        Just segAssets ->
          withSystemTempDirectory "pitcher-run-final" $ \tmpDir -> do
            segmentPaths <- forM (zip [(1 :: Int) ..] segAssets) $ \(ix, assetRef) ->
              let
                path = tmpDir </> ("segment_" <> show ix <> ".mp4")
              in do
              putStrLn $ "@[runFinalNode] s3 down asset: " <> show assetRef.eid <> " to " <> path
              Ao.downloadAssetToPath env.s3 assetRef.eid path
              pure path

            let
              outPath = tmpDir </> "final.mp4"
              gapS = fromMaybe env.video.gapDurationSeconds payload.gapSeconds
              fadeS = fromMaybe env.video.fadeDurationSeconds payload.fadeSeconds

            renderRes <- try $
              concatSegmentsWithGapsAndFades env.video gapS fadeS segmentPaths outPath
                :: IO (Either SomeException ())
            case renderRes of
              Left ex ->
                pure . Left . retryableFailure $
                  "ffmpeg final concat failed: " <> T.pack (show ex)
              Right () -> do
                upRes <- try $
                  As.uploadFileAsAsset
                    env.pool
                    env.s3
                    outPath
                    (assetNameForNode node "mp4")
                    "video/mp4"
                    ("final node " <> node.key)
                  :: IO (Either SomeException AssetRef)
                case upRes of
                  Left ex ->
                    pure . Left . retryableFailure $
                      "Final upload failed: " <> T.pack (show ex)
                  Right assetRef ->
                    pure . Right $
                      NodeComputeSuccess
                        { asset = assetRef
                        , requestEid = Nothing
                        , notes = Nothing
                        }

--------------------------------------------------------------------------------
-- Payload decode

parsePayload :: Ae.FromJSON a => Text -> Ae.Value -> Either Text a
parsePayload label val =
  first (\err -> "Could not decode " <> label <> ": " <> T.pack err) $ Ae.parseEither Ae.parseJSON val

--------------------------------------------------------------------------------
-- Upstream asset lookups

lookupInputAssetByNodeKey :: Pool -> Int64 -> Text -> IO (Maybe AssetRef)
lookupInputAssetByNodeKey pool graphUid nodeKey =
  runSessionOrThrow pool $
    statement (graphUid, nodeKey) Ts.selectInputAssetStmt >>= \case
      Nothing -> pure Nothing
      Just (assetUid, assetEid) -> pure . Just $ AssetRef { uid = assetUid, eid = assetEid }

loadDialogueSentences :: Pool -> Int64 -> IO [Text]
loadDialogueSentences pool dialogueUid = do
  rows <- runSessionOrThrow pool $ statement dialogueUid Ts.selectDialogueSentenceBodiesStmt
  pure $ map snd (Vc.toList rows)

loadDialogueVisualSentenceAnchors :: Pool -> Int64 -> IO [Maybe Int32]
loadDialogueVisualSentenceAnchors pool dialogueUid = do
  rows <- runSessionOrThrow pool $ statement dialogueUid Ts.selectDialogueVisualAnchorsStmt
  pure [ sentenceIx | (_, sentenceIx) <- Vc.toList rows ]

--------------------------------------------------------------------------------
-- Shot planning

buildTimedShotPlan
  :: [Text]
  -> [Maybe Int32]
  -> [FilePath]
  -> Double
  -> [(FilePath, Double)]
buildTimedShotPlan sentenceBodies visualAnchors imagePaths totalDuration
  | null imagePaths = []
  | length imagePaths == 1 = [(head imagePaths, max 0.8 totalDuration)]
  | otherwise =
      let starts = sentenceStartTimes sentenceBodies totalDuration

          indexedStarts =
            [ (img, sentenceStartFor starts (fromIntegral ix))
            | (img, Just ix) <- zip imagePaths visualAnchors
            ]

          unindexedImgs =
            [ img
            | (img, mbIx) <- zip imagePaths visualAnchors
            , isNothing mbIx
            ]

          evenStarts =
            case unindexedImgs of
              [] -> []
              xs ->
                let n = length xs
                    vals =
                      [ totalDuration * fromIntegral i / fromIntegral n
                      | i <- [0 .. n - 1]
                      ]
                in zip xs vals

          merged =
            L.sortOn snd (indexedStarts <> evenStarts)

          finalPairs =
            case merged of
              [] ->
                let n = length imagePaths
                    dur = max 0.8 (totalDuration / fromIntegral n)
                in [ (img, dur) | img <- imagePaths ]
              xs ->
                zipWith
                  (\(img, startT) nextStart -> (img, max 0.8 (nextStart - startT)))
                  xs
                  (map snd (drop 1 xs) <> [totalDuration])
      in finalPairs

sentenceStartTimes :: [Text] -> Double -> [Double]
sentenceStartTimes sentences totalDuration =
  let weights =
        map (fromIntegral . max 1 . T.length . squashWs) sentences
      totalW = max 1 (sum weights)
      durations =
        map (\w -> totalDuration * w / totalW) weights
  in scanl (+) 0 durations

sentenceStartFor :: [Double] -> Int -> Double
sentenceStartFor starts ix
  | ix <= 1 = 0
  | ix >= length starts = lastDef 0 starts
  | otherwise = starts !! (ix - 1)


--------------------------------------------------------------------------------
-- ffmpeg helpers

renderAudioOnlySegment :: VideoRenderCfg -> FilePath -> Double -> FilePath -> IO ()
renderAudioOnlySegment cfg audioPath dur outPath =
  runProcChecked cfg.ffmpegBin
    [ "-y"
    , "-f", "lavfi"
    , "-i", "color=c=black:s=" <> sizeArg cfg <> ":r=" <> show cfg.fps <> ":d=" <> show dur
    , "-i", audioPath
    , "-shortest"
    , "-c:v", "libx264"
    , "-pix_fmt", "yuv420p"
    , "-c:a", "aac"
    , outPath
    ]

createStillClip :: VideoRenderCfg -> FilePath -> Double -> FilePath -> IO ()
createStillClip cfg imagePath dur outPath =
  runProcChecked cfg.ffmpegBin
    [ "-y"
    , "-loop", "1"
    , "-i", imagePath
    , "-t", show dur
    , "-vf", baseVideoFilter cfg
    , "-an"
    , "-c:v", "libx264"
    , "-pix_fmt", "yuv420p"
    , outPath
    ]

concatStillClipsWithAudio :: VideoRenderCfg -> [FilePath] -> FilePath -> FilePath -> IO ()
concatStillClipsWithAudio cfg stillClips audioPath outPath =
  withSystemTempDirectory "pitcher-still-concat" $ \tmpDir -> do
    let listFile = tmpDir </> "list.txt"
    writeConcatListFile listFile stillClips
    runProcChecked cfg.ffmpegBin
      [ "-y"
      , "-f", "concat"
      , "-safe", "0"
      , "-i", listFile
      , "-i", audioPath
      , "-shortest"
      , "-c:v", "libx264"
      , "-pix_fmt", "yuv420p"
      , "-c:a", "aac"
      , outPath
      ]


concatSegmentsWithGapsAndFades :: VideoRenderCfg -> Double -> Double -> [FilePath] -> FilePath -> IO ()
concatSegmentsWithGapsAndFades cfg gapSeconds fadeSeconds segmentPaths outputPath =
  let
    nbrSegments = length segmentPaths
    videoCfg = VideoConfig { width = cfg.widthPx, height = cfg.heightPx, fps = cfg.fps, fadeDurationSeconds = fadeSeconds, gapDurationSeconds = gapSeconds }
    audioCfg = AudioConfig { sampleRate = 48000, channelLayout = "stereo", fadeDurationSeconds = fadeSeconds, gapDurationSeconds = gapSeconds }
    (filterGraph, videoMap, audioMap) = buildVideoFilterComplex videoCfg audioCfg nbrSegments
    args = concatMap (\p -> ["-i", p]) segmentPaths <>
            [ "-filter_complex", filterGraph
            , "-map", videoMap
            , "-map", audioMap
            , "-c:v", "libx264"
            , "-c:a", "aac"
            , "-movflags"
            , "+faststart"
            , outputPath
            ]
  in do
  putStrLn $ "@[concatSegmentsWithGapsAndFades] args: " <> show args
  runProcChecked cfg.ffmpegBin args


concatSegmentsWithGapsAndFadesB :: VideoRenderCfg -> Double -> Double -> [FilePath] -> FilePath -> IO ()
concatSegmentsWithGapsAndFadesB cfg gapSeconds fadeSeconds segmentPaths outPath =
  withSystemTempDirectory "pitcher-concat-segments" $ \tmpDir -> do
    normalized <- forM (zip [(1 :: Int) ..] segmentPaths) $ \(ix, src) ->
      let
        dst = tmpDir </> ("norm_" <> show ix <> ".mp4")
      in do
      normalizeSegmentWithFades cfg fadeSeconds ix (length segmentPaths) src dst
      pure dst

    gapClips <- forM [1 .. max 0 (length normalized - 1)] $ \ix -> do
      let gapPath = tmpDir </> ("gap_" <> show (ix :: Int) <> ".mp4")
      createGapClip cfg gapSeconds gapPath
      pure gapPath

    let
      interleaved = interleaveWithGaps normalized gapClips
      listFile = tmpDir </> "concat.txt"

    writeConcatListFile listFile interleaved
    runProcChecked cfg.ffmpegBin
      [ "-y"
      , "-f", "concat"
      , "-safe", "0"
      , "-i", listFile
      , "-c:v", "libx264"
      , "-pix_fmt", "yuv420p"
      , "-c:a", "aac"
      , outPath
      ]

normalizeSegmentWithFades
  :: VideoRenderCfg
  -> Double
  -> Int
  -> Int
  -> FilePath
  -> FilePath
  -> IO ()
normalizeSegmentWithFades cfg fadeSeconds ix total inPath outPath =
  let
    fade = show fadeSeconds
    vFadeIn = if ix == 1 then "" else ",fade=t=in:st=0:d=" <> fade
    aFadeIn = if ix == 1 then "" else ",afade=t=in:st=0:d=" <> fade
    vFadeOut = if ix == total then "" else ",reverse,fade=t=in:st=0:d=" <> fade <> ",reverse"
    aFadeOut = if ix == total then "" else ",areverse,afade=t=in:st=0:d=" <> fade <> ",areverse"
    vFilter = baseVideoFilter cfg <> vFadeIn <> vFadeOut
    aFilter = "asetpts=PTS-STARTPTS" <> aFadeIn <> aFadeOut
    filterGraph =
      "[0:v]" <> vFilter <> "[v];[0:a]" <> aFilter <> "[a]"
  in do
  runProcChecked cfg.ffmpegBin
    [ "-y"
    , "-i", inPath
    , "-filter_complex", filterGraph
    , "-map", "[v]"
    , "-map", "[a]"
    , "-c:v", "libx264"
    , "-pix_fmt", "yuv420p"
    , "-c:a", "aac"
    , outPath
    ]

--- From concatSegments utility:



data VideoConfig = VideoConfig {
    width :: Int
  , height :: Int
  , fps :: Int
  , fadeDurationSeconds :: Double
  , gapDurationSeconds :: Double
  }

data AudioConfig = AudioConfig {
    sampleRate :: Int
  , channelLayout :: String
  , fadeDurationSeconds :: Double
  , gapDurationSeconds :: Double
  }


buildVideoFilterComplex :: VideoConfig -> AudioConfig -> Int -> (String, String, String)
buildVideoFilterComplex videoCfg audioCfg nbrSegments =
  case nbrSegments of
    1 ->
      ( L.intercalate "; "
          [ normalizeVideo videoCfg 0 1 1
          , normalizeAudio audioCfg 0 1 1
          ]
      , "[v1]"
      , "[a1]"
      )
    _ ->
      ( L.intercalate "; " (videoNorms ++ audioNorms ++ gapDefs ++ [concatExpr])
      , "[vout]"
      , "[aout]"
      )
  where
  videoNorms = [ normalizeVideo videoCfg i (i + 1) nbrSegments | i <- [0 .. nbrSegments - 1] ]
  audioNorms = [ normalizeAudio audioCfg i (i + 1) nbrSegments | i <- [0 .. nbrSegments - 1] ]
  gapDefs = concat [ [ makeGapVideo videoCfg i, makeGapAudio audioCfg i ] | i <- [1 .. nbrSegments - 1] ]
  concatExpr = concat (buildVideoConcatParts nbrSegments) ++ "concat=n=" ++ show (2 * nbrSegments - 1) ++ ":v=1:a=1[vout][aout]"

normalizeVideo :: VideoConfig -> Int -> Int -> Int -> String
normalizeVideo cfg inputIdx outIdx totalCount =
  "[" ++ show inputIdx ++ ":v]"
    ++ "scale=" ++ show cfg.width ++ ":" ++ show cfg.height
    ++ ",setsar=1"
    ++ ",fps=" ++ show cfg.fps
    ++ ",format=yuv420p"
    ++ ",setpts=PTS-STARTPTS"
    ++ videoTransitionFilters cfg inputIdx totalCount
    ++ "[v" ++ show outIdx ++ "]"

normalizeAudio :: AudioConfig -> Int -> Int -> Int -> String
normalizeAudio cfg inputIdx outIdx totalCount =
  "[" ++ show inputIdx ++ ":a]"
    ++ "aformat=sample_rates=" ++ show cfg.sampleRate
    ++ ":channel_layouts=" ++ cfg.channelLayout
    ++ ",asetpts=PTS-STARTPTS"
    ++ audioTransitionFilters cfg inputIdx totalCount
    ++ "[a" ++ show outIdx ++ "]"


videoTransitionFilters :: VideoConfig -> Int -> Int -> String
videoTransitionFilters cfg inputIdx totalCount
  | totalCount <= 1 = ""
  | inputIdx == 0 =
      videoFadeOutAtEnd cfg
  | inputIdx == totalCount - 1 =
      videoFadeInAtStart cfg
  | otherwise =
      videoFadeInAtStart cfg ++ videoFadeOutAtEnd cfg

audioTransitionFilters :: AudioConfig -> Int -> Int -> String
audioTransitionFilters cfg inputIdx totalCount
  | totalCount <= 1 = ""
  | inputIdx == 0 =
      audioFadeOutAtEnd cfg
  | inputIdx == totalCount - 1 =
      audioFadeInAtStart cfg
  | otherwise =
      audioFadeInAtStart cfg ++ audioFadeOutAtEnd cfg

videoFadeInAtStart :: VideoConfig -> String
videoFadeInAtStart cfg = ",fade=t=in:st=0:d=" ++ show cfg.fadeDurationSeconds

videoFadeOutAtEnd :: VideoConfig -> String
videoFadeOutAtEnd cfg = ",reverse" ++ ",fade=t=in:st=0:d=" ++ show cfg.fadeDurationSeconds ++ ",reverse"

audioFadeInAtStart :: AudioConfig -> String
audioFadeInAtStart cfg =
 ",afade=t=in:st=0:d=" ++ show cfg.fadeDurationSeconds

audioFadeOutAtEnd :: AudioConfig -> String
audioFadeOutAtEnd cfg =
  ",areverse"
    ++ ",afade=t=in:st=0:d=" ++ show cfg.fadeDurationSeconds
    ++ ",areverse"

makeGapVideo :: VideoConfig -> Int -> String
makeGapVideo cfg gapIdx =
  "color=c=black:s="
    ++ show cfg.width ++ "x" ++ show cfg.height
    ++ ":r=" ++ show cfg.fps
    ++ ":d=" ++ show cfg.gapDurationSeconds
    ++ "[sv" ++ show gapIdx ++ "]"

makeGapAudio :: AudioConfig -> Int -> String
makeGapAudio cfg gapIdx =
  "anullsrc=r=" ++ show cfg.sampleRate
    ++ ":cl=" ++ cfg.channelLayout
    ++ ",atrim=duration=" ++ show cfg.gapDurationSeconds
    ++ "[sa" ++ show gapIdx ++ "]"

buildVideoConcatParts :: Int -> [String]
buildVideoConcatParts n =
  concatMap segment [1 .. n - 1] <> finalSegment n
  where
  segment i =
    [ "[v" <> show i <> "]", "[a" <> show i <> "]", "[sv" <> show i <> "]", "[sa" <> show i <> "]" ]
  finalSegment i = [ "[v" <> show i <> "]", "[a" <> show i <> "]" ]


createGapClip :: VideoRenderCfg -> Double -> FilePath -> IO ()
createGapClip cfg gapSeconds outPath =
  runProcChecked cfg.ffmpegBin
    [ "-y"
    , "-f", "lavfi"
    , "-i", "color=c=black:s=" <> sizeArg cfg <> ":r=" <> show cfg.fps <> ":d=" <> show gapSeconds
    , "-f", "lavfi"
    , "-i", "anullsrc=r=48000:cl=stereo"
    , "-shortest"
    , "-c:v", "libx264"
    , "-pix_fmt", "yuv420p"
    , "-c:a", "aac"
    , outPath
    ]


probeDurationSeconds :: FilePath -> FilePath -> IO Double
probeDurationSeconds ffprobeBin mediaPath =
  let
    args = [
        "-v", "error"
      , "-show_entries", "format=duration"
      , "-of", "default=noprint_wrappers=1:nokey=1"
      , mediaPath
      ]
  in do
  (ec, out, err) <- readProcessWithExitCode ffprobeBin args ""
  case ec of
    ExitSuccess -> case reads (trim out) of
        (val, _) : _ -> pure val
        _ -> throwIO . userError $ "@[probeDurationSeconds] Could not parse ffprobe duration from: " <> out
    ExitFailure _ -> throwIO . userError $ "@[probeDurationSeconds] ffprobe failed: " <> err


baseVideoFilter :: VideoRenderCfg -> String
baseVideoFilter cfg = "scale=" <> sizeArg cfg <> ",setsar=1,fps=" <> show cfg.fps <> ",format=yuv420p,setpts=PTS-STARTPTS"

sizeArg :: VideoRenderCfg -> String
sizeArg cfg = show cfg.widthPx <> ":" <> show cfg.heightPx

interleaveWithGaps :: [a] -> [a] -> [a]
interleaveWithGaps [] _ = []
interleaveWithGaps [x] _ = [x]
interleaveWithGaps (x:xs) (g:gs) = x : g : interleaveWithGaps xs gs
interleaveWithGaps xs [] = xs


writeConcatListFile :: FilePath -> [FilePath] -> IO ()
writeConcatListFile path files =
  let
    lineSpecs = T.unlines [ "file '" <> T.pack fp <> "'" | fp <- files ]
  in
    Bs.writeFile path . Te.encodeUtf8 $ lineSpecs


runProcChecked :: FilePath -> [String] -> IO ()
runProcChecked bin args = do
  (ec, out, err) <- readProcessWithExitCode bin args ""
  case ec of
    ExitSuccess -> pure ()
    ExitFailure _ -> throwIO . userError $ "@[runProcChecked] Process failed: " <> bin
              <> "\n" <> unlines args <> "\nstdout:\n" <> out <> "\nstderr:\n" <> err

