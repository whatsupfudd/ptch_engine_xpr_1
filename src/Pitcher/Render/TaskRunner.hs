{-# LANGUAGE DuplicateRecordFields #-}

module Pitcher.Render.TaskRunner
  ( TaskRunnerEnv(..)
  , VideoRenderCfg(..)
  , NodeComputeSuccess(..)
  , NodeComputeFailure(..)
  , runLeasedNodeToCompletion
  , dispatchNodeCompute
  ) where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
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
import Control.Monad (forM, forM_, void, when)

import qualified Data.ByteString as Bs
import Data.Int (Int32)
import qualified Data.List as L
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Te
import Data.UUID (UUID)
import qualified Data.UUID as Uu
import Data.Vector (Vector)
import qualified Data.Vector as V

import System.Directory (getFileSize)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)

import qualified Data.Aeson as Ae
import qualified Data.Aeson.Key as Aek
import qualified Data.Aeson.Types as Aet
import Data.Aeson ((.:), (.=))

import Hasql.Pool (Pool)
import Hasql.Session (statement)
import Hasql.Statement (Statement)
import qualified Hasql.TH as TH

import Assets.Types (AssetRef(..), S3Conn)
import qualified Assets.S3Ops as Ao
import qualified Assets.Store as As
import qualified AiSup.Types as Ai
import AiSup.Client
  ( downloadRemoteAiAsset
  , invokeForAsset
  , loginAiServer
  )
import DB.Helpers (runSessionOrThrow)
import Pitcher.Render.WorkerLease
  ( CompleteFailure(..)
  , CompleteSuccess(..)
  , LeasedNode(..)
  , RenderInput(..)
  , UpstreamAsset(..)
  , WorkerCaps(..)
  , completeNodeFailure
  , completeNodeSuccess
  , heartbeatNodeLease
  , loadNodeInputs
  , lookupUpstreamNodeAsset
  )
import Pitcher.Render.WorkTypes (assetNameForNode)
import qualified DB.TaskStmt as Ts
import Utils (lastDef, squashWs, trim)

--------------------------------------------------------------------------------
-- Environment

data TaskRunnerEnv = TaskRunnerEnv
  { pool :: Pool
  , s3 :: S3Conn
  , ai :: Ai.AiRunnerCfg
  , video :: VideoRenderCfg
  }

data VideoRenderCfg = VideoRenderCfg
  { ffmpegBin :: FilePath
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


data SectionTiming = SectionTiming {
    sentenceBodies :: [Text]
  , visualAnchors :: [Maybe Int32]
  }
  deriving (Eq, Show)


--------------------------------------------------------------------------------
-- Public entry points

runLeasedNodeToCompletion :: TaskRunnerEnv -> WorkerCaps -> LeasedNode -> IO Bool
runLeasedNodeToCompletion env caps node = do
  {-
  putStrLn $ "@[runLeasedNodeToCompletion] deriveKey=" <> T.unpack node.deriveKey
    <> " exec=" <> T.unpack node.exec
  -}

  withHeartbeat env.pool caps node $ do
    res <- try (dispatchNodeCompute env node) :: IO (Either SomeException (Either NodeComputeFailure NodeComputeSuccess))

    case res of
      Left ex -> completeNodeFailure env.pool CompleteFailure {
              nodeUid = node.nodeUid
            , owner = caps.owner
            , retryable = True
            , errorText = T.pack (show ex)
            , notes = Nothing
            , requestEid = Nothing
            }

      Right (Left failure) -> completeNodeFailure env.pool CompleteFailure {
              nodeUid = node.nodeUid
            , owner = caps.owner
            , retryable = failure.retryable
            , errorText = failure.errorText
            , notes = failure.notes
            , requestEid = failure.requestEid
            }

      Right (Right success) -> completeNodeSuccess env.pool CompleteSuccess {
              nodeUid = node.nodeUid
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
    "blender" -> pure . Left $
      fatalFailure "Blender execution is not implemented in this first-phase runner."

    other -> pure . Left . fatalFailure $ "Unsupported node.exec value: " <> other

--------------------------------------------------------------------------------
-- Heartbeat wrapper

withHeartbeat :: Pool -> WorkerCaps -> LeasedNode -> IO a -> IO a
withHeartbeat pool caps node action =
  bracket start stop (const action)
  where
    periodMicros :: Int
    periodMicros = max 1000000 ((max 3 (fromIntegral caps.leaseSeconds) `div` 3) * 1000000)

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
      threadDelay periodMicros
      mbStop <- tryReadMVar stopVar
      case mbStop of
        Just _ -> pure ()
        Nothing -> do
          _ <- heartbeatNodeLease pool node.nodeUid caps
          loop stopVar

--------------------------------------------------------------------------------
-- Audio generation
--
-- The audio source is now resolved by stable dialogue.eid through render_input.
-- No spoken text is stored in the render node.

runAudioNode :: TaskRunnerEnv -> LeasedNode -> IO (Either NodeComputeFailure NodeComputeSuccess)
runAudioNode env node = do
  inputs <- loadNodeInputs env.pool node.nodeUid

  case resolveSourceEid node inputs "dialogue" "dialogue" of
    Left err ->
      pure . Left $ fatalFailure err

    Right dialogueEid -> do
      sentences <- loadDialogueSentencesByEid env.pool dialogueEid
      if null sentences
        then
          pure . Left $
            fatalFailure ("Audio node dialogue has no sentences: " <> Uu.toText dialogueEid)
        else do
          let spokenText = T.intercalate " " sentences
              params =
                maybe
                  Ae.Null
                  (\voice -> Ae.object ["voice" .= voice])
                  env.ai.ttsVoice

          aiClient <- loginAiServer env.ai

          invokeRes <-
            try $
              invokeForAsset
                aiClient
                env.ai.ttsFunctionEid
                params
                (Ae.toJSON spokenText)
              :: IO (Either SomeException (UUID, UUID))

          case invokeRes of
            Left ex ->
              pure . Left . retryableFailure $
                "@[runAudioNode] TTS invoke failed: " <> T.pack (show ex)

            Right (reqEid, remoteAssetEid) ->
              withSystemTempDirectory "pitcher-run-audio" $ \tmpDir -> do
                let localPath = tmpDir </> "audio.mp3"

                dlRes <-
                  try $
                    downloadRemoteAiAsset aiClient remoteAssetEid localPath
                  :: IO (Either SomeException ())

                case dlRes of
                  Left ex ->
                    pure . Left . retryableFailure $
                      "TTS asset download failed: " <> T.pack (show ex)

                  Right () -> do
                    upRes <-
                      try $
                        As.uploadFileAsAsset
                          env.pool
                          env.s3
                          localPath
                          (assetNameForNode node "mp3")
                          "audio/mpeg"
                          ("audio node " <> node.deriveKey)
                      :: IO (Either SomeException AssetRef)

                    case upRes of
                      Left ex ->
                        pure . Left . retryableFailure $
                          "TTS asset upload failed: " <> T.pack (show ex)

                      Right assetRef ->
                        pure . Right $
                          NodeComputeSuccess
                            { asset = assetRef
                            , requestEid = Just reqEid
                            , notes = Just ("remoteAsset=" <> Uu.toText remoteAssetEid)
                            }

--------------------------------------------------------------------------------
-- Image generation
--
-- The image prompt source is now resolved by stable dialogue_visual.eid.

runImageNode :: TaskRunnerEnv -> LeasedNode -> IO (Either NodeComputeFailure NodeComputeSuccess)
runImageNode env node = do
  inputs <- loadNodeInputs env.pool node.nodeUid

  case resolveSourceEid node inputs "visual" "visual" of
    Left err ->
      pure . Left $ fatalFailure err

    Right visualEid -> do
      mbDescription <- loadVisualDescriptionByEid env.pool visualEid
      case mbDescription of
        Nothing ->
          pure . Left $
            fatalFailure ("Image node visual source not found: " <> Uu.toText visualEid)

        Just description -> do
          (prefixes, postfixes) <- loadVizContextsByVisualEid env.pool visualEid
          aiClient <- loginAiServer env.ai

          let
            prefix = if V.null prefixes then "" else snd (V.head prefixes)
            postfix = if V.null postfixes then "" else snd (V.head postfixes)
            fullPrompt = prefix <> description <> postfix
            params = Ae.object ["model" .= env.ai.imageModel]

          invokeRes <- try $ invokeForAsset aiClient env.ai.imageFunctionEid params (Ae.toJSON fullPrompt)
                :: IO (Either SomeException (UUID, UUID))

          case invokeRes of
            Left ex -> pure . Left . retryableFailure $ "Image invoke failed: " <> T.pack (show ex)

            Right (reqEid, remoteAssetEid) ->
              withSystemTempDirectory "pitcher-run-image" $ \tmpDir -> do
                let
                  localPath = tmpDir </> "image.png"

                dlRes <-
                  try $ downloadRemoteAiAsset aiClient remoteAssetEid localPath :: IO (Either SomeException ())

                case dlRes of
                  Left ex -> pure . Left . retryableFailure $ "@[runImageNode] Image asset download failed: " <> T.pack (show ex)

                  Right () -> do
                    upRes <- try $ As.uploadFileAsAsset env.pool env.s3 localPath
                          (assetNameForNode node "png") "image/png" ("image node " <> node.deriveKey)
                          :: IO (Either SomeException AssetRef)

                    case upRes of
                      Left ex -> pure . Left . retryableFailure $ "Image asset upload failed: " <> T.pack (show ex)

                      Right assetRef ->
                        pure . Right $ NodeComputeSuccess { 
                              asset = assetRef
                            , requestEid = Just reqEid
                            , notes = Just ("remoteAsset=" <> Uu.toText remoteAssetEid)
                            }


--------------------------------------------------------------------------------
-- Segment fusion
--
-- Segment nodes use:
--
--   source_eid / source input: dialogue.eid
--   node inputs role=audio: upstream audio derive key
--   node inputs role=image: upstream image derive keys, in visual order

runSegmentNodeA :: TaskRunnerEnv -> LeasedNode -> IO (Either NodeComputeFailure NodeComputeSuccess)
runSegmentNodeA env node = do
  inputs <- loadNodeInputs env.pool node.nodeUid
  case resolveDialogueEidForSegment node inputs of
    Left err -> pure . Left $ fatalFailure err

    Right dialogueEid -> do
      case resolveNodeInputKeys inputs "audio" of
        [] -> pure . Left $ fatalFailure ("Segment node has no audio input: " <> node.deriveKey)
        [audioKey] -> do
            mbAudioAsset <- lookupUpstreamAssetRef env node audioKey
            imageAssetsRes <- mapM (lookupUpstreamAssetRef env node) (resolveNodeInputKeys inputs "image")
            case (mbAudioAsset, sequence imageAssetsRes) of
              (Nothing, _) -> pure . Left $
                  fatalFailure ("Segment node is missing upstream audio asset: " <> audioKey)
              (_, Nothing) -> pure . Left $
                  fatalFailure ("Segment node is missing one or more upstream image assets: " <> node.deriveKey)
              (Just audioAsset, Just imageAssets) ->
                renderSegmentWithAssets env node dialogueEid audioAsset imageAssets

        -- In case there's ever many audio segments generated for a single dialogue:
        manyAudioKeys -> do
          withSystemTempDirectory "pitcher-audio-concat" $ \tmpDir -> do
            eiFilePath <- prepareSegmentAudio env node tmpDir manyAudioKeys
            case eiFilePath of
              Left err -> pure $ Left err
              Right concatAudioFile -> do
                imageAssetsRes <- mapM (lookupUpstreamAssetRef env node) (resolveNodeInputKeys inputs "image")
                case sequence imageAssetsRes of
                  Nothing -> pure . Left $ fatalFailure ("Segment node is missing one or more upstream image assets: " <> node.deriveKey)
                  Just imageAssets ->
                    renderSegmentWithConcatAudio env node dialogueEid concatAudioFile imageAssets


runSegmentNode :: TaskRunnerEnv -> LeasedNode -> IO (Either NodeComputeFailure NodeComputeSuccess)
runSegmentNode env node = do
  inputs <- loadNodeInputs env.pool node.nodeUid

  let
    dialogueEids = resolveDialogueEidsForSegment inputs
    audioKeys = resolveNodeInputKeys inputs "audio"
    imageKeys = resolveNodeInputKeys inputs "image"

  if null dialogueEids then
    pure . Left $ fatalFailure ("Segment node has no dialogue source inputs: " <> node.deriveKey)
  else if null audioKeys then
    pure . Left $ fatalFailure ("Segment node has no audio node inputs: " <> node.deriveKey)
  else
    withSystemTempDirectory "pitcher-run-segment" $ \tmpDir -> do
      audioPathRes <- prepareSegmentAudio env node tmpDir audioKeys

      case audioPathRes of
        Left err -> pure (Left err)

        Right sectionAudioPath -> do
          imageAssetsRes <- mapM (lookupUpstreamAssetRef env node) imageKeys
          case sequence imageAssetsRes of
            Nothing -> pure . Left $
                fatalFailure ("Segment node is missing one or more upstream image assets: " <> node.deriveKey)

            Just imageAssets -> do
              imagePaths <- forM (zip [(1 :: Int) ..] imageAssets) $ \(ix, assetRef) ->
                  let
                    imgPath = tmpDir </> ("img_" <> show ix <> ".png")
                  in do
                  Ao.downloadAssetToPath env.s3 assetRef.eid imgPath
                  pure imgPath
              renderSegmentSectionWithFiles env node dialogueEids tmpDir sectionAudioPath imagePaths


renderSegmentSectionWithFiles :: TaskRunnerEnv -> LeasedNode -> [UUID] -> FilePath
              -> FilePath -> [FilePath]
              -> IO (Either NodeComputeFailure NodeComputeSuccess)
renderSegmentSectionWithFiles env node dialogueEids tmpDir audioPath imagePaths = do
  let
    outPath = tmpDir </> "segment.mp4"
  putStrLn $ "@[renderSegmentSectionWithFiles] audioPath: " <> audioPath <> ", dialogueEids="
      <> show dialogueEids <> ", images=" <> show imagePaths

  durationRes <- try $ probeDurationSeconds env.video.ffprobeBin audioPath :: IO (Either SomeException Double)

  case durationRes of
    Left ex -> pure . Left . retryableFailure $ "ffprobe failed for segment audio: " <> T.pack (show ex)
    Right audioDuration -> do
      sectionTiming <- loadSectionTiming env.pool node dialogueEids

      let
        shotPlan = buildTimedShotPlan sectionTiming.sentenceBodies sectionTiming.visualAnchors imagePaths audioDuration

      renderRes <- try $
          case imagePaths of
            [] -> renderAudioOnlySegment env.video audioPath audioDuration outPath

            _ -> do
              stillClips <- forM (zip [(1 :: Int) ..] shotPlan) $ \(ix, (imgPath, dur)) ->
                  let
                    clipPath = tmpDir </> ("still_" <> show ix <> ".mp4")
                  in do
                  createStillClip env.video imgPath dur clipPath
                  pure clipPath
              concatStillClipsWithAudio env.video stillClips audioPath outPath
        :: IO (Either SomeException ())

      case renderRes of
        Left ex -> pure . Left . retryableFailure $ "ffmpeg segment render failed: " <> T.pack (show ex)

        Right () -> do
          upRes <- try $ As.uploadFileAsAsset env.pool env.s3 outPath
                (assetNameForNode node "mp4") "video/mp4" ("segment node " <> node.deriveKey)
                :: IO (Either SomeException AssetRef)
          case upRes of
            Left ex -> pure . Left . retryableFailure $ "Segment upload failed: " <> T.pack (show ex)
            Right assetRef -> pure . Right $ NodeComputeSuccess { asset = assetRef, requestEid = Nothing, notes = Nothing }


loadSectionTiming :: Pool -> LeasedNode -> [UUID] -> IO SectionTiming
loadSectionTiming pool node dialogueEids = do
  sentenceBlocks <- forM dialogueEids $ \dialogueEid -> loadDialogueSentencesByEid pool dialogueEid

  let
    sentenceBodies = concat sentenceBlocks
    sentenceCounts = map (fromIntegral . length) sentenceBlocks :: [Int32]
    sentenceOffsets = init (scanl (+) 0 sentenceCounts)
    dialogueOffsets = zip dialogueEids sentenceOffsets
    visualEids = paramUuidList "visualEids" node.params

  visualAnchorRows <- mapM (loadVisualOwnerAndAnchorByEid pool) visualEids

  let
    visualAnchors = [ fmap (+ sentenceOffsetFor dialogueOffsets ownerDialogueEid) localSentenceOrd
              | (ownerDialogueEid, localSentenceOrd) <- visualAnchorRows
      ]

  pure SectionTiming { sentenceBodies = sentenceBodies, visualAnchors = visualAnchors }



sentenceOffsetFor :: [(UUID, Int32)] -> UUID -> Int32
sentenceOffsetFor offsets dialogueEid =
  fromMaybe 0 (lookup dialogueEid offsets)

paramUuidList :: Text -> Ae.Value -> [UUID]
paramUuidList name val =
  fromMaybe [] $
    Aet.parseMaybe parser val
  where
    parser =
      Ae.withObject "params" $ \o -> do
        rawValues <- o .: Aek.fromText name
        pure [ uuidVal | raw <- rawValues, Just uuidVal <- [Uu.fromText raw] ]


prepareSegmentAudio :: TaskRunnerEnv -> LeasedNode -> FilePath -> [Text] -> IO (Either NodeComputeFailure FilePath)
prepareSegmentAudio env node tmpDir audioKeys = do
  assetMaybes <- mapM (lookupUpstreamAssetRef env node) audioKeys

  case sequence assetMaybes of
    Nothing -> pure . Left $
        fatalFailure ("Segment node is missing one or more upstream audio assets: " <> node.deriveKey)
    Just audioAssets -> do
      audioPaths <- forM (zip [(1 :: Int) ..] audioAssets) $ \(ix, assetRef) ->
          let
            path = tmpDir </> ("audio_" <> show ix <> ".mp3")
          in do
          Ao.downloadAssetToPath env.s3 assetRef.eid path
          pure path
      case audioPaths of
        [] -> pure . Left $ fatalFailure ("Segment node has no audio inputs: " <> node.deriveKey)
        [single] -> pure (Right single)
        manyPaths ->
          let
            outPath = tmpDir </> "section_audio.mp3"
          in do
          concatAudioFiles env.video.ffmpegBin manyPaths outPath
          pure (Right outPath)


concatAudioFiles :: FilePath -> [FilePath] -> FilePath -> IO ()
concatAudioFiles ffmpegBin inputPaths outputPath = do
  putStrLn $ "@[concatAudioFiles] inputPaths: " <> show inputPaths <> " to " <> outputPath
  withSystemTempDirectory "pitcher-audio-concat" $ \tmpDir -> do
    let
      listFile = tmpDir </> "audio-list.txt"
    writeConcatListFile listFile inputPaths
    runProcChecked ffmpegBin
      [ "-y"
      , "-f", "concat"
      , "-safe", "0"
      , "-i", listFile
      , "-c:a", "libmp3lame"
      , "-q:a", "2"
      , outputPath
      ]


renderSegmentWithAssets :: TaskRunnerEnv -> LeasedNode -> UUID -> AssetRef -> [AssetRef]
                            -> IO (Either NodeComputeFailure NodeComputeSuccess)
renderSegmentWithAssets env node dialogueEid audioAsset imageAssets =
  withSystemTempDirectory "pitcher-run-segment" $ \tmpDir -> do
    let
      audioPath = tmpDir </> "dialogue.mp3"
      outPath = tmpDir </> "segment.mp4"
    putStrLn $ "@[renderSegmentWithAssets] audioPath: " <> audioPath <> ", images: " <> show imageAssets <> " to " <> outPath
    Ao.downloadAssetToPath env.s3 audioAsset.eid audioPath
    imagePaths <- forM (zip [(1 :: Int) ..] imageAssets) $ \(ix, assetRef) ->
      let
        imgPath = tmpDir </> ("img_" <> show ix <> ".png")
      in do
      Ao.downloadAssetToPath env.s3 assetRef.eid imgPath
      pure imgPath
    renderSegmentWithFiles env node dialogueEid outPath audioPath imagePaths


renderSegmentWithConcatAudio :: TaskRunnerEnv -> LeasedNode -> UUID -> FilePath -> [AssetRef] -> IO (Either NodeComputeFailure NodeComputeSuccess)
renderSegmentWithConcatAudio env node dialogueEid audioPath imageAssets =
  withSystemTempDirectory "pitcher-run-segment" $ \tmpDir -> do
    putStrLn $ "@[renderSegmentWithConcatAudio] audioPath: " <> audioPath <> ", images: " <> show imageAssets
    imagePaths <- forM (zip [(1 :: Int) ..] imageAssets) $ \(ix, assetRef) ->
      let
        imgPath = tmpDir </> ("img_" <> show ix <> ".png")
      in do
      Ao.downloadAssetToPath env.s3 assetRef.eid imgPath
      pure imgPath
    renderSegmentWithFiles env node dialogueEid tmpDir audioPath imagePaths


renderSegmentWithFiles :: TaskRunnerEnv -> LeasedNode -> UUID -> FilePath -> FilePath -> [FilePath]
              -> IO (Either NodeComputeFailure NodeComputeSuccess)
renderSegmentWithFiles env node dialogueEid tmpDir audioPath imagePaths = do
    let
      outPath = tmpDir </> "segment.mp4"

    putStrLn $ "@[renderSegmentWithFiles] audioPath: " <> audioPath <> ", images: " <> show imagePaths

    durationRes <- try $ probeDurationSeconds env.video.ffprobeBin audioPath :: IO (Either SomeException Double)
    case durationRes of
      Left ex -> pure . Left . retryableFailure $ "ffprobe failed for segment audio: " <> T.pack (show ex)
      Right audioDuration -> do
        sentenceBodies <- loadDialogueSentencesByEid env.pool dialogueEid
        visualAnchors <- loadDialogueVisualSentenceAnchorsByDialogueEid env.pool dialogueEid

        let
          shotPlan = buildTimedShotPlan sentenceBodies visualAnchors imagePaths audioDuration

        renderRes <- try $ case imagePaths of
              [] -> renderAudioOnlySegment env.video audioPath audioDuration outPath
              _ -> do
                stillClips <- forM (zip [(1 :: Int) ..] shotPlan) $ \(ix, (imgPath, dur)) ->
                    let
                      clipPath = tmpDir </> ("still_" <> show ix <> ".mp4")
                    in do
                    createStillClip env.video imgPath dur clipPath
                    pure clipPath

                concatStillClipsWithAudio env.video stillClips audioPath outPath
            :: IO (Either SomeException ())

        case renderRes of
          Left ex -> pure . Left . retryableFailure $ "ffmpeg segment render failed: " <> T.pack (show ex)

          Right () -> do
            upRes <- try $ As.uploadFileAsAsset env.pool env.s3 outPath
                  (assetNameForNode node "mp4") "video/mp4" ("segment node " <> node.deriveKey)
                :: IO (Either SomeException AssetRef)

            case upRes of
              Left ex -> pure . Left . retryableFailure $ "Segment upload failed: " <> T.pack (show ex)

              Right assetRef -> pure . Right $ NodeComputeSuccess {
                      asset = assetRef
                    , requestEid = Nothing
                    , notes = Nothing
                    }


--------------------------------------------------------------------------------
-- Finalization

runFinalNode :: TaskRunnerEnv -> LeasedNode -> IO (Either NodeComputeFailure NodeComputeSuccess)
runFinalNode env node = do
  inputs <- loadNodeInputs env.pool node.nodeUid
  let
    segmentKeys = resolveNodeInputKeys inputs "segment"

  if null segmentKeys then
    pure . Left $ fatalFailure ("Final node has no segment inputs: " <> node.deriveKey)
  else do
    -- putStrLn $ "@[runFinalNode] segmentKeys: " <> show segmentKeys
    segmentAssetsRes <- mapM (lookupUpstreamAssetRef env node) segmentKeys
    case sequence segmentAssetsRes of
      Nothing -> pure . Left $
        fatalFailure ("Final node is missing one or more upstream segment assets: " <> node.deriveKey)
      Just segmentAssets ->
        withSystemTempDirectory "pitcher-run-final" $ \tmpDir -> do
          segmentPaths <-
            forM (zip [(1 :: Int) ..] segmentAssets) $ \(ix, assetRef) -> do
              let path = tmpDir </> ("segment_" <> show ix <> ".mp4")
              Ao.downloadAssetToPath env.s3 assetRef.eid path
              pure path

          let
            outPath = tmpDir </> "final.mp4"
            gapS = paramDoubleWithDefault "gapSeconds" env.video.gapDurationSeconds node.params
            fadeS = paramDoubleWithDefault "fadeSeconds" env.video.fadeDurationSeconds node.params

          renderRes <- try $
              concatSegmentsWithGapsAndFades env.video gapS fadeS segmentPaths outPath
              :: IO (Either SomeException ())

          case renderRes of
            Left ex -> pure . Left . retryableFailure $ "ffmpeg final concat failed: " <> T.pack (show ex)

            Right () -> do
              upRes <- try $ As.uploadFileAsAsset env.pool env.s3 outPath
                    (assetNameForNode node "mp4") "video/mp4" ("final node " <> node.deriveKey)
                    :: IO (Either SomeException AssetRef)

              case upRes of
                Left ex -> pure . Left . retryableFailure $ "Final upload failed: " <> T.pack (show ex)
                Right assetRef -> pure . Right $
                    NodeComputeSuccess { asset = assetRef, requestEid = Nothing, notes = Nothing }


--------------------------------------------------------------------------------
-- Input resolution

resolveSourceEid :: LeasedNode -> [RenderInput] -> Text -> Text -> Either Text UUID
resolveSourceEid node inputs expectedRefKind expectedRole =
  case node.sourceEid of
    Just eid -> Right eid

    Nothing ->
      let
        matchingInputs =
          [ eid |
                input <- inputs
              , input.inputKind == "source", input.refKind == expectedRefKind, input.role == Just expectedRole
              , Just eid <- [input.refEid]
          ]
      in
      case matchingInputs of
        [] -> Left $ "Missing source input for node " <> node.deriveKey
              <> "; expected refKind=" <> expectedRefKind
              <> " role=" <> expectedRole
        eid : _ -> Right eid


resolveDialogueEidForSegment :: LeasedNode -> [RenderInput] -> Either Text UUID
resolveDialogueEidForSegment node inputs =
  resolveSourceEid node inputs "dialogue" "dialogue"


resolveDialogueEidsForSegment :: [RenderInput] -> [UUID]
resolveDialogueEidsForSegment inputs =
  [ eid | input <- L.sortOn (.ord) inputs
      , input.inputKind == "source", input.refKind == "dialogue", input.role == Just "dialogue"
      , Just eid <- [input.refEid]
  ]

resolveNodeInputKeys :: [RenderInput] -> Text -> [Text]
resolveNodeInputKeys inputs expectedRole =
  [ key
  | input <- L.sortOn (.ord) inputs
  , input.inputKind == "node"
  , input.refKind == "render_node"
  , input.role == Just expectedRole
  , Just key <- [input.refDeriveKey]
  ]


lookupUpstreamAssetRef :: TaskRunnerEnv -> LeasedNode -> Text -> IO (Maybe AssetRef)
lookupUpstreamAssetRef env node deriveKey = do
  -- putStrLn $ "@[lookupUpstreamAssetRef] deriveKey: " <> T.unpack deriveKey
  mb <- lookupUpstreamNodeAsset env.pool node.renderJobUid deriveKey
  pure $ fmap (\asset -> AssetRef { uid = asset.assetUid, eid = asset.assetEid } ) mb


paramDoubleWithDefault :: Text -> Double -> Ae.Value -> Double
paramDoubleWithDefault name def val =
  fromMaybe def $ Aet.parseMaybe parser val
  where
  parser = Ae.withObject "params" $ \o -> o .: Aek.fromText name

--------------------------------------------------------------------------------
-- Source lookup SQL

loadDialogueSentencesByEid :: Pool -> UUID -> IO [Text]
loadDialogueSentencesByEid pool dialogueEid = do
  rows <- runSessionOrThrow "selectDialogueSentenceBodiesByEidStmt" pool $
      statement dialogueEid Ts.selectDialogueSentenceBodiesByEidStmt
  pure [ body | (_ord, body) <- V.toList rows ]

loadDialogueVisualSentenceAnchorsByDialogueEid :: Pool -> UUID -> IO [Maybe Int32]
loadDialogueVisualSentenceAnchorsByDialogueEid pool dialogueEid = do
  rows <- runSessionOrThrow "selectDialogueVisualAnchorsByDialogueEidStmt" pool $
      statement dialogueEid Ts.selectDialogueVisualAnchorsByDialogueEidStmt
  pure [ sentenceIx | (_ord, sentenceIx) <- V.toList rows ]


loadVisualDescriptionByEid :: Pool -> UUID -> IO (Maybe Text)
loadVisualDescriptionByEid pool visualEid =
  runSessionOrThrow "selectVisualDescriptionByEidStmt" pool $
    statement visualEid Ts.selectVisualDescriptionByEidStmt


loadVisualOwnerAndAnchorByEid :: Pool -> UUID -> IO (UUID, Maybe Int32)
loadVisualOwnerAndAnchorByEid pool visualEid =
  runSessionOrThrow "selectVisualOwnerAndAnchorByEidStmt" pool $
      statement visualEid Ts.selectVisualOwnerAndAnchorByEidStmt

loadVizContextsByVisualEid :: Pool -> UUID -> IO (Vector (Int32, Text), Vector (Int32, Text))
loadVizContextsByVisualEid pool visualEid = do
  rows <- runSessionOrThrow "selectVizContextsByVisualEidStmt" pool $
      statement visualEid Ts.selectVizContextsByVisualEid
  let
    prefixes = V.map (\(kind, seqnum, content) -> (seqnum, content)) $ V.filter (\(kind, _, _) -> kind == "prefix") rows
    postfixes = V.map (\(kind, seqnum, content) -> (seqnum, content)) $ V.filter (\(kind, _, _) -> kind == "postfix") rows
  pure (prefixes, postfixes)

--------------------------------------------------------------------------------
-- Shot planning

buildTimedShotPlan :: [Text] -> [Maybe Int32] -> [FilePath] -> Double -> [(FilePath, Double)]
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
      in
        finalPairs

sentenceStartTimes :: [Text] -> Double -> [Double]
sentenceStartTimes sentences totalDuration =
  let weights =
        map (fromIntegral . max 1 . T.length . squashWs) sentences
      totalW = max 1 (sum weights)
      durations =
        map (\w -> totalDuration * w / totalW) weights
  in
    scanl (+) 0 durations

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
    , "-c:v", "h264_videotoolbox"
    , "-pix_fmt", "yuv420p"
    , "-c:a", "aac_at"
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
    , "-c:v", "h264_videotoolbox"
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
      , "-c:v", "h264_videotoolbox"
      , "-pix_fmt", "yuv420p"
      , "-c:a", "aac_at"
      , outPath
      ]

concatSegmentsWithGapsAndFades :: VideoRenderCfg -> Double -> Double -> [FilePath] -> FilePath -> IO ()
concatSegmentsWithGapsAndFades cfg gapSeconds fadeSeconds segmentPaths outputPath =
  let
    nbrSegments = length segmentPaths
    videoCfg =
      VideoConfig
        { width = cfg.widthPx
        , height = cfg.heightPx
        , fps = cfg.fps
        , fadeDurationSeconds = fadeSeconds
        , gapDurationSeconds = gapSeconds
        }

    audioCfg =
      AudioConfig
        { sampleRate = 48000
        , channelLayout = "stereo"
        , fadeDurationSeconds = fadeSeconds
        , gapDurationSeconds = gapSeconds
        }

    (filterGraph, videoMap, audioMap) =
      buildVideoFilterComplex videoCfg audioCfg nbrSegments

    args =
      concatMap (\p -> ["-i", p]) segmentPaths
        <>
          [ "-filter_complex", filterGraph
          , "-map", videoMap
          , "-map", audioMap
          , "-c:v", "h264_videotoolbox"
          , "-c:a", "aac_at"
          , "-movflags", "+faststart"
          , outputPath
          ]
  in
    runProcChecked cfg.ffmpegBin args

data VideoConfig = VideoConfig
  { width :: Int
  , height :: Int
  , fps :: Int
  , fadeDurationSeconds :: Double
  , gapDurationSeconds :: Double
  }

data AudioConfig = AudioConfig
  { sampleRate :: Int
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
      ( L.intercalate "; " (videoNorms <> audioNorms <> gapDefs <> [concatExpr])
      , "[vout]"
      , "[aout]"
      )
  where
    videoNorms =
      [ normalizeVideo videoCfg i (i + 1) nbrSegments
      | i <- [0 .. nbrSegments - 1]
      ]

    audioNorms =
      [ normalizeAudio audioCfg i (i + 1) nbrSegments
      | i <- [0 .. nbrSegments - 1]
      ]

    gapDefs =
      concat
        [ [ makeGapVideo videoCfg i
          , makeGapAudio audioCfg i
          ]
        | i <- [1 .. nbrSegments - 1]
        ]

    concatExpr =
      concat (buildVideoConcatParts nbrSegments)
        <> "concat=n="
        <> show (2 * nbrSegments - 1)
        <> ":v=1:a=1[vout][aout]"

normalizeVideo :: VideoConfig -> Int -> Int -> Int -> String
normalizeVideo cfg inputIdx outIdx totalCount =
  "[" <> show inputIdx <> ":v]"
    <> "scale=" <> show cfg.width <> ":" <> show cfg.height
    <> ",setsar=1"
    <> ",fps=" <> show cfg.fps
    <> ",format=yuv420p"
    <> ",setpts=PTS-STARTPTS"
    <> videoTransitionFilters cfg inputIdx totalCount
    <> "[v" <> show outIdx <> "]"

normalizeAudio :: AudioConfig -> Int -> Int -> Int -> String
normalizeAudio cfg inputIdx outIdx totalCount =
  "[" <> show inputIdx <> ":a]"
    <> "aformat=sample_rates=" <> show cfg.sampleRate
    <> ":channel_layouts=" <> cfg.channelLayout
    <> ",asetpts=PTS-STARTPTS"
    <> audioTransitionFilters cfg inputIdx totalCount
    <> "[a" <> show outIdx <> "]"

videoTransitionFilters :: VideoConfig -> Int -> Int -> String
videoTransitionFilters cfg inputIdx totalCount
  | totalCount <= 1 = ""
  | inputIdx == 0 = videoFadeOutAtEnd cfg
  | inputIdx == totalCount - 1 = videoFadeInAtStart cfg
  | otherwise = videoFadeInAtStart cfg <> videoFadeOutAtEnd cfg

audioTransitionFilters :: AudioConfig -> Int -> Int -> String
audioTransitionFilters cfg inputIdx totalCount
  | totalCount <= 1 = ""
  | inputIdx == 0 = audioFadeOutAtEnd cfg
  | inputIdx == totalCount - 1 = audioFadeInAtStart cfg
  | otherwise = audioFadeInAtStart cfg <> audioFadeOutAtEnd cfg

videoFadeInAtStart :: VideoConfig -> String
videoFadeInAtStart cfg =
  ",fade=t=in:st=0:d=" <> show cfg.fadeDurationSeconds

videoFadeOutAtEnd :: VideoConfig -> String
videoFadeOutAtEnd cfg =
  ",reverse,fade=t=in:st=0:d=" <> show cfg.fadeDurationSeconds <> ",reverse"

audioFadeInAtStart :: AudioConfig -> String
audioFadeInAtStart cfg =
  ",afade=t=in:st=0:d=" <> show cfg.fadeDurationSeconds

audioFadeOutAtEnd :: AudioConfig -> String
audioFadeOutAtEnd cfg =
  ",areverse,afade=t=in:st=0:d=" <> show cfg.fadeDurationSeconds <> ",areverse"

makeGapVideo :: VideoConfig -> Int -> String
makeGapVideo cfg gapIdx =
  "color=c=black:s="
    <> show cfg.width
    <> "x"
    <> show cfg.height
    <> ":r="
    <> show cfg.fps
    <> ":d="
    <> show cfg.gapDurationSeconds
    <> "[sv"
    <> show gapIdx
    <> "]"

makeGapAudio :: AudioConfig -> Int -> String
makeGapAudio cfg gapIdx =
  "anullsrc=r="
    <> show cfg.sampleRate
    <> ":cl="
    <> cfg.channelLayout
    <> ",atrim=duration="
    <> show cfg.gapDurationSeconds
    <> "[sa"
    <> show gapIdx
    <> "]"

buildVideoConcatParts :: Int -> [String]
buildVideoConcatParts n =
  concatMap segment [1 .. n - 1] <> finalSegment n
  where
    segment i =
      [ "[v" <> show i <> "]"
      , "[a" <> show i <> "]"
      , "[sv" <> show i <> "]"
      , "[sa" <> show i <> "]"
      ]

    finalSegment i =
      [ "[v" <> show i <> "]"
      , "[a" <> show i <> "]"
      ]

probeDurationSeconds :: FilePath -> FilePath -> IO Double
probeDurationSeconds ffprobeBin mediaPath = do
  let args =
        [ "-v", "error"
        , "-show_entries", "format=duration"
        , "-of", "default=noprint_wrappers=1:nokey=1"
        , mediaPath
        ]

  (ec, out, err) <- readProcessWithExitCode ffprobeBin args ""

  case ec of
    ExitSuccess ->
      case reads (trim out) of
        (val, _) : _ ->
          pure val
        _ ->
          throwIO . userError $
            "@[probeDurationSeconds] Could not parse ffprobe duration from: " <> out

    ExitFailure _ ->
      throwIO . userError $
        "@[probeDurationSeconds] ffprobe failed: " <> err

baseVideoFilter :: VideoRenderCfg -> String
baseVideoFilter cfg =
  "scale="
    <> sizeArg cfg
    <> ",setsar=1,fps="
    <> show cfg.fps
    <> ",format=yuv420p,setpts=PTS-STARTPTS"

sizeArg :: VideoRenderCfg -> String
sizeArg cfg =
  show cfg.widthPx <> ":" <> show cfg.heightPx

writeConcatListFile :: FilePath -> [FilePath] -> IO ()
writeConcatListFile path files =
  Bs.writeFile path . Te.encodeUtf8 $
    T.unlines
      [ "file '" <> T.pack fp <> "'"
      | fp <- files
      ]

runProcChecked :: FilePath -> [String] -> IO ()
runProcChecked bin args = do
  (ec, out, err) <- readProcessWithExitCode bin args ""
  case ec of
    ExitSuccess ->
      pure ()

    ExitFailure _ ->
      throwIO . userError $
        "@[runProcChecked] Process failed: "
          <> bin
          <> "\n"
          <> unlines args
          <> "\nstdout:\n"
          <> out
          <> "\nstderr:\n"
          <> err