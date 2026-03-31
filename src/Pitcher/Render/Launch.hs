{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Pitcher.Render.Launch
  ( RenderEnv(..)
  , RenderParallelism(..)
  , RenderOutcome(..)
  , launchRender
  ) where

import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Concurrent.STM
  ( TMVar
  , TQueue
  , TVar
  , atomically
  , modifyTVar'
  , newEmptyTMVarIO
  , newTQueueIO
  , newTVarIO
  , putTMVar
  , readTMVar
  , readTVarIO
  , readTQueue
  , readTVar
  , tryPutTMVar
  , writeTQueue
  )
import Control.Exception (SomeException, bracket, finally, throwIO, try)
import Control.Monad (forM, forM_, forever, replicateM_, unless, void, when)

import Data.Bifunctor (first)
import qualified Data.ByteString as Bs
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as Lbs
import Data.Int (Int32, Int64)
import qualified Data.List as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Mp
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, mapMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Te
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import qualified Data.UUID as Uu
import qualified Data.UUID.V4 as Uu4
import qualified Data.Vector as Vc

import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , getFileSize
  , removeFile
  )
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO (IOMode(..), withFile, hFileSize)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)

import GHC.Generics (Generic)

import qualified Data.Aeson as Ae
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson.Types as Ae

import qualified Data.Conduit as C

import Hasql.Pool (Pool)
import qualified Hasql.Pool as Hp
import Hasql.Session (Session, statement)
import Hasql.Statement (Statement)
import qualified Hasql.TH as TH

import qualified Network.HTTP.Client as Hc
import qualified Network.HTTP.Client.TLS as Hct
import qualified Network.HTTP.Types.Header as Hh
import qualified Network.Minio as Mn


import Options.Runtime (AiConfig (..))
import Pitcher.Render.Types (RenderEnv (..), RenderParallelism (..), RenderOutcome (..))
import Pitcher.NarrationTypes (DialogueRender (..), VisualRender (..), NarrationRender (..))
import qualified AiSup.Client as Ai
import AiSup.Types (AiClient (..), AiRunnerCfg (..))
import qualified Assets.Types as At
import qualified Assets.S3Ops as At
import qualified Assets.Store as As
import DB.Helpers (runSessionOrThrow)
import qualified DB.LaunchStmt as Ls
import qualified DB.LaunchOps as Lo
import Pitcher.Render.TaskTypes
import Utils (squashWs, lastDef, sigText, maybeToList, tshow, trim)


dialogueSpokenText :: DialogueRender -> Text
dialogueSpokenText dlg =
  T.intercalate " " dlg.sentences


stateToMap :: PersistedRenderState -> Map Text TaskSnapshot
stateToMap st =
  Mp.fromList [ (t.key, t) | t <- st.tasks ]


replaceTask :: TaskSnapshot -> PersistedRenderState -> PersistedRenderState
replaceTask task st =
  let mp = Mp.insert task.key task (stateToMap st)
  in st { tasks = Mp.elems mp }


lookupTask :: Text -> PersistedRenderState -> Maybe TaskSnapshot
lookupTask key st = Mp.lookup key (stateToMap st)


--------------------------------------------------------------------------------
-- Plan / tasks


buildRenderPlan :: RenderEnv -> NarrationRender -> RenderPlan
buildRenderPlan env narration =
  let audioPairs =
        [ (audioTaskKey dlg, AudioTask dlg (audioSourceSig env dlg))
        | dlg <- narration.dialogues
        ]
      imagePairs =
        [ (imageTaskKey dlg vis, ImageTask dlg vis (imageSourceSig env dlg vis))
        | dlg <- narration.dialogues
        , vis <- dlg.visuals
        ]
      segmentPairs =
        [ ( segmentTaskKey dlg
          , SegmentTask
              { dialogue = dlg
              , audioKey = audioTaskKey dlg
              , imageKeys = [ imageTaskKey dlg vis | vis <- dlg.visuals ]
              , sourceSig = segmentSourceSig env dlg
              }
          )
        | dlg <- narration.dialogues
        ]
      finalT =
        FinalTask
          { segmentKeys = [ segmentTaskKey dlg | dlg <- narration.dialogues ]
          , sourceSig = finalSourceSig env narration
          }
  in RenderPlan
      { narration = narration
      , audioTasks = Mp.fromList audioPairs
      , imageTasks = Mp.fromList imagePairs
      , segmentTasks = Mp.fromList segmentPairs
      , finalTask = finalT
      }

audioSourceSig :: RenderEnv -> DialogueRender -> Text
audioSourceSig env dlg =
  sigText $
    Ae.object
      [ "v" .= env.renderVersionTag
      , "stage" .= ("audio" :: Text)
      , "ttsFunctionEid" .= Uu.toText env.aiCfg.ttsFunctionEid
      , "voice" .= env.aiCfg.ttsVoice
      , "dialogueUid" .= dlg.uid
      , "emotion" .= dlg.emotion
      , "spokenText" .= dialogueSpokenText dlg
      ]

imageSourceSig :: RenderEnv -> DialogueRender -> VisualRender -> Text
imageSourceSig env dlg vis =
  sigText $
    Ae.object
      [ "v" .= env.renderVersionTag
      , "stage" .= ("image" :: Text)
      , "imageFunctionEid" .= Uu.toText env.aiCfg.imageFunctionEid
      , "dialogueUid" .= dlg.uid
      , "visualOrd" .= vis.ord
      , "sentenceIx" .= vis.sentenceIx
      , "description" .= vis.description
      , "promptPrefix" .= env.aiCfg.imagePromptPrefix
      , "promptPostfix" .= env.aiCfg.imagePromptPostfix
      ]

segmentSourceSig :: RenderEnv -> DialogueRender -> Text
segmentSourceSig env dlg =
  sigText $
    Ae.object
      [ "v" .= env.renderVersionTag
      , "stage" .= ("segment" :: Text)
      , "dialogueUid" .= dlg.uid
      , "audioSourceSig" .= audioSourceSig env dlg
      , "imageSourceSigs" .=
          [ imageSourceSig env dlg vis
          | vis <- dlg.visuals
          ]
      , "width" .= env.widthPx
      , "height" .= env.heightPx
      , "fps" .= env.fps
      ]

finalSourceSig :: RenderEnv -> NarrationRender -> Text
finalSourceSig env narration =
  sigText $
    Ae.object
      [ "v" .= env.renderVersionTag
      , "stage" .= ("final" :: Text)
      , "segmentSourceSigs" .=
          [ segmentSourceSig env dlg
          | dlg <- narration.dialogues
          ]
      , "width" .= env.widthPx
      , "height" .= env.heightPx
      , "fps" .= env.fps
      , "gapDurationSeconds" .= env.gapDurationSeconds
      , "fadeDurationSeconds" .= env.fadeDurationSeconds
      ]

--------------------------------------------------------------------------------
-- Runtime

data RenderRuntime = RenderRuntime
  { audioQ :: TQueue AudioTask
  , imageQ :: TQueue ImageTask
  , segmentQ :: TQueue SegmentTask
  , finalQ :: TQueue FinalTask
  , stateVar :: TVar PersistedRenderState
  , stopVar :: TVar Bool
  , outcomeVar :: TMVar RenderOutcome
  , jobUid :: Int64
  }

newRuntime :: Int64 -> PersistedRenderState -> IO RenderRuntime
newRuntime jobUid initState = do
  audioQ <- newTQueueIO
  imageQ <- newTQueueIO
  segmentQ <- newTQueueIO
  finalQ <- newTQueueIO
  stateVar <- newTVarIO initState
  stopVar <- newTVarIO False
  outcomeVar <- newEmptyTMVarIO
  pure RenderRuntime
    { audioQ = audioQ
    , imageQ = imageQ
    , segmentQ = segmentQ
    , finalQ = finalQ
    , stateVar = stateVar
    , stopVar = stopVar
    , outcomeVar = outcomeVar
    , jobUid = jobUid
    }

--------------------------------------------------------------------------------
-- Top-level launch

launchRender :: RenderEnv -> UUID -> Pool -> IO RenderOutcome
launchRender env narrationEid pool = do
  mbUid <- runSessionOrThrow pool $ statement narrationEid Ls.selectNarrationUidStmt
  case mbUid of
    Nothing -> pure $ RenderFailed { jobUid = 0
        , reason = "Narration " <> (T.pack . show) narrationEid <> " not found." 
        }
    Just narrationUid -> do
      narration <- Lo.loadNarrationRender pool narrationUid
      if null narration.dialogues then
        throwIO . userError $ "Narration " <> show narrationEid <> " has no dialogues."
      else do
        jobUid <- Lo.loadOrCreateRenderJob pool narrationUid
        let
          plan = buildRenderPlan env narration
          par = env.parallelism
        initialState <- buildInitialState pool jobUid plan
        rt <- newRuntime jobUid initialState
        Lo.persistRenderJobState pool rt.jobUid "running" initialState Nothing
        replicateM_ par.audioWorkers . void . forkIO $ audioWorkerLoop env pool rt
        replicateM_ par.imageWorkers . void . forkIO $ imageWorkerLoop env pool rt
        replicateM_ par.segmentWorkers . void . forkIO $ segmentWorkerLoop env pool rt
        void . forkIO $ finalWorkerLoop env pool rt
        void . forkIO $ coordinatorLoop env pool rt plan
        result <- atomically $ readTMVar rt.outcomeVar
        case result of
          RenderSucceeded { finalAssetEid = eid } -> do
            st <- readTVarIO rt.stateVar
            mbUid <- Lo.lookupAssetUidByEidIO pool eid
            Lo.persistRenderJobState pool rt.jobUid "completed" st mbUid
          RenderFailed { reason = msg } -> do
            st <- readTVarIO rt.stateVar
            Lo.persistRenderJobState pool rt.jobUid ("failed:" <> msg) st Nothing

        atomically $ modifyTVar' rt.stopVar (const True)
        pure result


--------------------------------------------------------------------------------
-- Coordinator

coordinatorLoop :: RenderEnv -> Pool -> RenderRuntime -> RenderPlan -> IO ()
coordinatorLoop env pool rt plan =
  forever $ do
    stopped <- readTVarIO rt.stopVar
    unless stopped $ do
      stepCoordinator env pool rt plan
      threadDelay 500000

stepCoordinator :: RenderEnv -> Pool -> RenderRuntime -> RenderPlan -> IO ()
stepCoordinator env pool rt plan = do
  st <- readTVarIO rt.stateVar

  case failureInState st of
    Just err | env.failFast -> do
      let failure = RenderFailed rt.jobUid err
      void . atomically $ tryPutTMVar rt.outcomeVar failure
      atomically $ modifyTVar' rt.stopVar (const True)
    _ -> pure ()

  let enqueueAudio =
        [ task
        | (k, task) <- Mp.toList plan.audioTasks
        , isPending k st
        ]

      enqueueImages =
        [ task
        | (k, task) <- Mp.toList plan.imageTasks
        , isPending k st
        ]

      enqueueSegments =
        [ task
        | (k, task) <- Mp.toList plan.segmentTasks
        , isPending k st
        , depsDone task.audioKey task.imageKeys st
        ]

      finalPending =
        isPending finalTaskKey st
          && all (\k -> isDone k st) plan.finalTask.segmentKeys

  unless (null enqueueAudio && null enqueueImages && null enqueueSegments && not finalPending) $ do
    atomically $ do
      forM_ enqueueAudio $ \task -> do
        writeTQueue rt.audioQ task
        modifyTVar' rt.stateVar (replaceTaskQueued (audioTaskKey task.dialogue))
      forM_ enqueueImages $ \task -> do
        writeTQueue rt.imageQ task
        modifyTVar' rt.stateVar (replaceTaskQueued (imageTaskKey task.dialogue task.visual))
      forM_ enqueueSegments $ \task -> do
        writeTQueue rt.segmentQ task
        modifyTVar' rt.stateVar (replaceTaskQueued (segmentTaskKey task.dialogue))
      when finalPending $ do
        writeTQueue rt.finalQ plan.finalTask
        modifyTVar' rt.stateVar (replaceTaskQueued finalTaskKey)

    st1 <- readTVarIO rt.stateVar
    Lo.persistRenderJobState pool rt.jobUid "running" st1 Nothing

  st2 <- readTVarIO rt.stateVar
  when (isDone finalTaskKey st2) $ do
    case lookupTask finalTaskKey st2 >>= (.assetEid) of
      Nothing ->
        pure ()
      Just eid -> do
        let ok = RenderSucceeded rt.jobUid eid
        void . atomically $ tryPutTMVar rt.outcomeVar ok
        atomically $ modifyTVar' rt.stopVar (const True)

replaceTaskQueued :: Text -> PersistedRenderState -> PersistedRenderState
replaceTaskQueued k st =
  case lookupTask k st of
    Nothing -> st
    Just task ->
      replaceTask task { status = QueuedTS } st

failureInState :: PersistedRenderState -> Maybe Text
failureInState st =
  let errs =
        [ err
        | t <- st.tasks
        , t.status == FailedTS
        , err <- maybeToList t.errorText
        ]
  in case errs of
      [] -> Nothing
      x : _ -> Just x

isPending :: Text -> PersistedRenderState -> Bool
isPending k st =
  case lookupTask k st of
    Just t -> t.status == PendingTS
    Nothing -> False

isDone :: Text -> PersistedRenderState -> Bool
isDone k st =
  case lookupTask k st of
    Just t -> t.status == DoneTS || t.status == SkippedTS
    Nothing -> False

depsDone :: Text -> [Text] -> PersistedRenderState -> Bool
depsDone audioKey imageKeys st =
  isDone audioKey st && all (`isDone` st) imageKeys

--------------------------------------------------------------------------------
-- Initial state / reuse

buildInitialState :: Pool -> Int64 -> RenderPlan -> IO PersistedRenderState
buildInitialState pool jobUid plan = do
  audioStates <- forM (Mp.toList plan.audioTasks) $ \(k, task) ->
    resolveInitialTask pool jobUid k AudioTK (Just task.dialogue.uid) Nothing task.sourceSig

  imageStates <- forM (Mp.toList plan.imageTasks) $ \(k, task) ->
    resolveInitialTask pool jobUid k ImageTK (Just task.dialogue.uid) (Just task.visual.ord) task.sourceSig

  segmentStates <- forM (Mp.toList plan.segmentTasks) $ \(k, task) ->
    resolveInitialTask pool jobUid k SegmentTK (Just task.dialogue.uid) Nothing task.sourceSig

  finalState <- resolveInitialTask pool jobUid finalTaskKey FinalTK Nothing Nothing plan.finalTask.sourceSig

  pure PersistedRenderState { 
      narrationUid = plan.narration.narrationUid
    , tasks = audioStates <> imageStates <> segmentStates <> [finalState]
    , finalAssetEid =
        case finalState.assetEid of
          Just eid | finalState.status == DoneTS -> Just eid
          _ -> Nothing
    }


resolveInitialTask
  :: Pool
  -> Int64
  -> Text
  -> TaskKind
  -> Maybe Int64
  -> Maybe Int32
  -> Text
  -> IO TaskSnapshot
resolveInitialTask pool jobUid key kind mbDialogue mbVisualOrd sourceSig = do
  reusable <- Lo.lookupReusableArtifact pool jobUid (kindText kind) mbDialogue mbVisualOrd sourceSig
  pure $
    case reusable of
      Just (_, assetEid) ->
        TaskSnapshot
          { key = key
          , kind = kind
          , sourceSig = sourceSig
          , status = DoneTS
          , assetEid = Just assetEid
          , requestEid = Nothing
          , errorText = Nothing
          }
      Nothing ->
        TaskSnapshot
          { key = key
          , kind = kind
          , sourceSig = sourceSig
          , status = PendingTS
          , assetEid = Nothing
          , requestEid = Nothing
          , errorText = Nothing
          }

kindText :: TaskKind -> Text
kindText = \case
  AudioTK -> "audio"
  ImageTK -> "image"
  SegmentTK -> "segment"
  FinalTK -> "final"

--------------------------------------------------------------------------------
-- Workers

audioWorkerLoop :: RenderEnv -> Pool -> RenderRuntime -> IO ()
audioWorkerLoop env pool rt =
  workerLoop rt.stopVar rt.audioQ $ \task -> do
    let key = audioTaskKey task.dialogue
    setTaskRunning pool rt key
    aiClient <- Ai.loginAiServer env.aiCfg
    res <- try $ renderAudioTask env pool aiClient task :: IO (Either SomeException At.AssetRef)
    case res of
      Left ex ->
        setTaskFailed pool rt key (T.pack $ show ex)
      Right assetRef ->
        setTaskDone pool rt key assetRef Nothing

imageWorkerLoop :: RenderEnv -> Pool -> RenderRuntime -> IO ()
imageWorkerLoop env pool rt =
  workerLoop rt.stopVar rt.imageQ $ \task -> do
    let key = imageTaskKey task.dialogue task.visual
    setTaskRunning pool rt key
    aiClient <- Ai.loginAiServer env.aiCfg
    res <- try $ renderImageTask env pool aiClient task :: IO (Either SomeException At.AssetRef)
    case res of
      Left ex ->
        setTaskFailed pool rt key (T.pack $ show ex)
      Right assetRef ->
        setTaskDone pool rt key assetRef Nothing

segmentWorkerLoop :: RenderEnv -> Pool -> RenderRuntime -> IO ()
segmentWorkerLoop env pool rt =
  workerLoop rt.stopVar rt.segmentQ $ \task -> do
    let key = segmentTaskKey task.dialogue
    setTaskRunning pool rt key
    res <- try $ renderSegmentTask env pool rt task :: IO (Either SomeException At.AssetRef)
    case res of
      Left ex ->
        setTaskFailed pool rt key (T.pack $ show ex)
      Right assetRef ->
        setTaskDone pool rt key assetRef Nothing

finalWorkerLoop :: RenderEnv -> Pool -> RenderRuntime -> IO ()
finalWorkerLoop env pool rt =
  workerLoop rt.stopVar rt.finalQ $ \task -> do
    setTaskRunning pool rt finalTaskKey
    res <- try $ renderFinalTask env pool rt task :: IO (Either SomeException At.AssetRef)
    case res of
      Left ex ->
        setTaskFailed pool rt finalTaskKey (T.pack $ show ex)
      Right assetRef ->
        setTaskDone pool rt finalTaskKey assetRef Nothing

workerLoop :: TVar Bool -> TQueue a -> (a -> IO ()) -> IO ()
workerLoop stopVar queue action =
  forever $ do
    stop <- readTVarIO stopVar
    unless stop $ do
      item <- atomically $ readTQueue queue
      action item

setTaskRunning :: Pool -> RenderRuntime -> Text -> IO ()
setTaskRunning pool rt key = do
  atomically $
    modifyTVar' rt.stateVar $
      setTaskField key (\t -> t { status = RunningTS, errorText = Nothing })
  st <- readTVarIO rt.stateVar
  Lo.persistRenderJobState pool rt.jobUid "running" st Nothing

setTaskFailed :: Pool -> RenderRuntime -> Text -> Text -> IO ()
setTaskFailed pool rt key err = do
  atomically $
    modifyTVar' rt.stateVar $
      setTaskField key (\t -> t { status = FailedTS, errorText = Just err })
  st <- readTVarIO rt.stateVar
  Lo.persistRenderJobState pool rt.jobUid "running" st Nothing

setTaskDone :: Pool -> RenderRuntime -> Text -> At.AssetRef -> Maybe UUID -> IO ()
setTaskDone pool rt key assetRef mbReq = do
  atomically $
    modifyTVar' rt.stateVar $
      (\st ->
        let st1 = setTaskField key (\t ->
                    t { status = DoneTS
                      , assetEid = Just assetRef.eid
                      , requestEid = mbReq
                      , errorText = Nothing
                      }
                  ) st
        in if key == finalTaskKey
            then (st1 :: PersistedRenderState) { finalAssetEid = Just assetRef.eid }
            else st1
      )
  st <- readTVarIO rt.stateVar
  Lo.persistRenderJobState pool rt.jobUid "running" st Nothing

setTaskField
  :: Text
  -> (TaskSnapshot -> TaskSnapshot)
  -> PersistedRenderState
  -> PersistedRenderState
setTaskField key f st =
  case lookupTask key st of
    Nothing -> st
    Just task -> replaceTask (f task) st

--------------------------------------------------------------------------------
-- Stage execution

renderAudioTask :: RenderEnv -> Pool -> AiClient -> AudioTask -> IO At.AssetRef
renderAudioTask env pool aiClient task =
  let
    content = dialogueSpokenText task.dialogue
    params = maybe Ae.Null (\voice -> Ae.object [ "voice" .= voice ]) env.aiCfg.ttsVoice
  in do
  (reqEid, remoteAssetEid) <- Ai.invokeForAsset aiClient env.aiCfg.ttsFunctionEid params (Ae.toJSON content)
  localAsset <- copyRemoteAssetIntoLocalStore env.s3Conn pool aiClient remoteAssetEid
          ("pitcher-audio-" <> tshow task.dialogue.uid <> ".mp3") "pitcher:audio"

  Lo.writeArtifactRecord pool rtJobless "audio" (Just task.dialogue.uid)
          Nothing task.sourceSig "done" (Just localAsset.uid) (Just localAsset.eid)
          (Just reqEid) (Just $ "remoteAsset=" <> Uu.toText remoteAssetEid)

  pure localAsset
  where
  rtJobless = 0 -- replaced by caller-side save via job-specific reusable keying


renderImageTask :: RenderEnv -> Pool -> AiClient -> ImageTask -> IO At.AssetRef
renderImageTask env pool aiClient task = do
  let
    prompt = env.aiCfg.imagePromptPrefix <> task.visual.description <> env.aiCfg.imagePromptPostfix
    params = Ae.object [ "model" .= env.aiCfg.imageModel ]

  (reqEid, remoteAssetEid) <- Ai.invokeForAsset aiClient env.aiCfg.imageFunctionEid params (Ae.toJSON prompt)
  localAsset <- copyRemoteAssetIntoLocalStore env.s3Conn pool aiClient remoteAssetEid ("pitcher-image-" <> tshow task.dialogue.uid <> "-" <> tshow task.visual.ord <> ".png") "pitcher:image"

  Lo.writeArtifactRecord
    pool
    0
    "image"
    (Just task.dialogue.uid)
    (Just task.visual.ord)
    task.sourceSig
    "done"
    (Just localAsset.uid)
    (Just localAsset.eid)
    (Just reqEid)
    (Just $ "remoteAsset=" <> Uu.toText remoteAssetEid)

  pure localAsset

renderSegmentTask :: RenderEnv -> Pool -> RenderRuntime -> SegmentTask -> IO At.AssetRef
renderSegmentTask env pool rt task = do
  st <- readTVarIO rt.stateVar
  let audioEid =
        fromMaybe
          (error "Missing audio asset eid for segment task")
          (lookupTask task.audioKey st >>= (.assetEid))

      imageEids =
        [ eid
        | key <- task.imageKeys
        , eid <- maybeToList (lookupTask key st >>= (.assetEid))
        ]

  withSystemTempDirectory "pitcher-render-segment" $ \tmpDir -> do
    let audioPath = tmpDir </> "dialogue.mp3"
        outPath = tmpDir </> "segment.mp4"

    At.downloadAssetToPath env.s3Conn audioEid audioPath
    forM_ (zip [(1 :: Int) ..] imageEids) $ \(ix, eid) ->
      At.downloadAssetToPath env.s3Conn eid (tmpDir </> ("img_" <> show ix <> ".png"))

    audioDuration <- probeDurationSeconds env.ffprobeBin audioPath
    let imagePaths =
          [ tmpDir </> ("img_" <> show ix <> ".png")
          | ix <- [1 .. length imageEids]
          ]

    case imagePaths of
      [] ->
        renderAudioOnlySegment env audioPath audioDuration outPath
      _ -> do
        let shotPlan = buildShotPlan task.dialogue imagePaths audioDuration
        stillClips <- forM (zip [(1 :: Int) ..] shotPlan) $ \(ix, (imgPath, dur)) -> do
          let clipPath = tmpDir </> ("still_" <> show ix <> ".mp4")
          createStillClip env imgPath dur clipPath
          pure clipPath
        concatStillClipsWithAudio env stillClips audioPath outPath

    assetRef <-
      As.uploadFileAsAsset
        pool
        env.s3Conn
        outPath
        ("segment-" <> tshow task.dialogue.uid <> ".mp4")
        "video/mp4"
        "pitcher:segment"

    Lo.writeArtifactRecord
      pool
      rt.jobUid
      "segment"
      (Just task.dialogue.uid)
      Nothing
      task.sourceSig
      "done"
      (Just assetRef.uid)
      (Just assetRef.eid)
      Nothing
      Nothing

    pure assetRef

renderFinalTask :: RenderEnv -> Pool -> RenderRuntime -> FinalTask -> IO At.AssetRef
renderFinalTask env pool rt task =
  withSystemTempDirectory "pitcher-render-final" $ \tmpDir -> do
    st <- readTVarIO rt.stateVar

    let segmentEids =
          [ eid
          | key <- task.segmentKeys
          , eid <- maybeToList (lookupTask key st >>= (.assetEid))
          ]

    when (null segmentEids) $
      throwIO . userError $
        "renderFinalTask: no segment assets available."

    segmentPaths <- forM (zip [(1 :: Int) ..] segmentEids) $ \(ix, eid) -> do
      let dst = tmpDir </> ("segment_" <> show ix <> ".mp4")
      At.downloadAssetToPath env.s3Conn eid dst
      pure dst

    outPath <- pure $ tmpDir </> "final.mp4"
    concatSegmentsWithGapsAndFades env segmentPaths outPath

    assetRef <-
      As.uploadFileAsAsset
        pool
        env.s3Conn
        outPath
        ("narration-" <> tshow rt.jobUid <> ".mp4")
        "video/mp4"
        "pitcher:final"

    Lo.writeArtifactRecord
      pool
      rt.jobUid
      "final"
      Nothing
      Nothing
      task.sourceSig
      "done"
      (Just assetRef.uid)
      (Just assetRef.eid)
      Nothing
      Nothing

    pure assetRef

--------------------------------------------------------------------------------
-- Shot planning

buildShotPlan :: DialogueRender -> [FilePath] -> Double -> [(FilePath, Double)]
buildShotPlan dialogue imagePaths totalDuration
  | null imagePaths = []
  | length imagePaths == 1 = [(head imagePaths, max 0.8 totalDuration)]
  | otherwise =
      let
        visuals = dialogue.visuals
        sentenceStarts = sentenceStartTimes dialogue.sentences totalDuration
        indexedStarts =
            [ (img, sentenceStartFor sentenceStarts (fromIntegral ix)) | (img, vis) <- zip imagePaths visuals , ix <- maybeToList vis.sentenceIx ]
        unindexedImgs =
            [ img | (img, vis) <- zip imagePaths visuals, isNothing vis.sentenceIx ]
        evenStarts = case unindexedImgs of
              [] -> []
              xs ->
                let n = length xs
                    starts =
                      [ totalDuration * fromIntegral i / fromIntegral n
                      | i <- [0 .. n - 1]
                      ]
                in zip xs starts
        merged = L.sortBy (comparing snd) (indexedStarts <> evenStarts)
        withDur =
            zipWith
              (\(img, startT) nextStart ->
                (img, max 0.8 (nextStart - startT))
              )
              merged
              (map snd (drop 1 merged) <> [totalDuration])
      in
      if null withDur then
        let
          n = length imagePaths
          dur = max 0.8 (totalDuration / fromIntegral n)
        in
        [ (img, dur) | img <- imagePaths ]
      else withDur


sentenceStartTimes :: [Text] -> Double -> [Double]
sentenceStartTimes sentences totalDuration =
  let
    weights = map (fromIntegral . max 1 . T.length . squashWs) sentences
    totalW = max 1 (sum weights)
    durations = map (\w -> totalDuration * w / totalW) weights
  in
  scanl (+) 0 durations


sentenceStartFor :: [Double] -> Int -> Double
sentenceStartFor starts ix
  | ix <= 1 = 0
  | ix >= length starts = lastDef 0 starts
  | otherwise = starts !! (ix - 1)


--------------------------------------------------------------------------------
-- ffmpeg helpers

renderAudioOnlySegment :: RenderEnv -> FilePath -> Double -> FilePath -> IO ()
renderAudioOnlySegment env audioPath dur outPath =
  runProcChecked env.ffmpegBin
    [ "-y"
    , "-f", "lavfi"
    , "-i", "color=c=black:s=" <> sizeArg env <> ":r=" <> show env.fps <> ":d=" <> show dur
    , "-i", audioPath
    , "-shortest"
    , "-c:v", "libx264"
    , "-pix_fmt", "yuv420p"
    , "-c:a", "aac"
    , outPath
    ]

createStillClip :: RenderEnv -> FilePath -> Double -> FilePath -> IO ()
createStillClip env imagePath dur outPath =
  runProcChecked env.ffmpegBin
    [ "-y"
    , "-loop", "1"
    , "-i", imagePath
    , "-t", show dur
    , "-vf", baseVideoFilter env
    , "-an"
    , "-c:v", "libx264"
    , "-pix_fmt", "yuv420p"
    , outPath
    ]

concatStillClipsWithAudio :: RenderEnv -> [FilePath] -> FilePath -> FilePath -> IO ()
concatStillClipsWithAudio env stillClips audioPath outPath =
  withSystemTempDirectory "pitcher-still-concat" $ \tmpDir ->
    let
      listFile = tmpDir </> "list.txt"
    in do
    writeConcatListFile listFile stillClips
    runProcChecked env.ffmpegBin
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

concatSegmentsWithGapsAndFades :: RenderEnv -> [FilePath] -> FilePath -> IO ()
concatSegmentsWithGapsAndFades env segmentPaths outPath =
  withSystemTempDirectory "pitcher-concat-segments" $ \tmpDir -> do
    normalized <- forM (zip [(1 :: Int) ..] segmentPaths) $ \(ix, src) ->
      let
        dst = tmpDir </> ("norm_" <> show ix <> ".mp4")
      in do
      normalizeSegmentWithFades env ix (length segmentPaths) src dst
      pure dst

    gapClips <- forM [1 .. max 0 (length normalized - 1)] $ \ix ->
      let
        gapPath = tmpDir </> ("gap_" <> show (ix :: Int) <> ".mp4")
      in do
      createGapClip env gapPath
      pure gapPath

    let
      interleaved = interleaveWithGaps normalized gapClips
      listFile = tmpDir </> "concat.txt"

    writeConcatListFile listFile interleaved

    runProcChecked env.ffmpegBin
      [ "-y"
      , "-f", "concat"
      , "-safe", "0"
      , "-i", listFile
      , "-c:v", "libx264"
      , "-pix_fmt", "yuv420p"
      , "-c:a", "aac"
      , outPath
      ]


normalizeSegmentWithFades :: RenderEnv -> Int -> Int -> FilePath -> FilePath -> IO ()
normalizeSegmentWithFades env ix total inPath outPath =
  let
    fade = show env.fadeDurationSeconds
    vFadeIn = if ix == 1 then "" else ",fade=t=in:st=0:d=" <> fade
    aFadeIn = if ix == 1 then "" else ",afade=t=in:st=0:d=" <> fade
    vFadeOut = if ix == total then "" else ",reverse,fade=t=in:st=0:d=" <> fade <> ",reverse"
    aFadeOut = if ix == total then "" else ",areverse,afade=t=in:st=0:d=" <> fade <> ",areverse"
    vFilter = baseVideoFilter env <> vFadeIn <> vFadeOut
    aFilter = "asetpts=PTS-STARTPTS" <> aFadeIn <> aFadeOut
    filterGraph = "[0:v]" <> vFilter <> "[v];[0:a]" <> aFilter <> "[a]"
    params = [
          "-y"
        , "-i", inPath
        , "-filter_complex", filterGraph
        , "-map", "[v]"
        , "-map", "[a]"
        , "-c:v", "libx264"
        , "-pix_fmt", "yuv420p"
        , "-c:a", "aac"
        , outPath
        ]
  in do
  runProcChecked env.ffmpegBin params


createGapClip :: RenderEnv -> FilePath -> IO ()
createGapClip env outPath =
  let
    params = [ "-y"
        , "-f", "lavfi"
        , "-i", "color=c=black:s=" <> sizeArg env <> ":r=" <> show env.fps <> ":d=" <> show env.gapDurationSeconds
        , "-f", "lavfi"
        , "-i", "anullsrc=r=48000:cl=stereo"
        , "-shortest"
        , "-c:v", "libx264"
        , "-pix_fmt", "yuv420p"
        , "-c:a", "aac"
        , outPath
        ]
  in
  runProcChecked env.ffmpegBin params


probeDurationSeconds :: FilePath -> FilePath -> IO Double
probeDurationSeconds ffprobeBin mediaPath = do
  (ec, out, err) <-
    let
      params =
        [ "-v", "error"
        , "-show_entries", "format=duration"
        , "-of", "default=noprint_wrappers=1:nokey=1"
        , mediaPath
        ]
    in
    readProcessWithExitCode ffprobeBin params ""


  case ec of
    ExitSuccess -> case reads (trim out) of
        (val, _) : _ -> pure val
        _ -> throwIO . userError $ "Could not parse ffprobe duration from: " <> out
    ExitFailure _ -> throwIO . userError $ "ffprobe failed: " <> err

baseVideoFilter :: RenderEnv -> String
baseVideoFilter env = "scale=" <> sizeArg env <> ",setsar=1,fps=" <> show env.fps <> ",format=yuv420p,setpts=PTS-STARTPTS"

sizeArg :: RenderEnv -> String
sizeArg env = show env.widthPx <> ":" <> show env.heightPx

interleaveWithGaps :: [a] -> [a] -> [a]
interleaveWithGaps [] _ = []
interleaveWithGaps [x] _ = [x]
interleaveWithGaps (x:xs) (g:gs) = x : g : interleaveWithGaps xs gs
interleaveWithGaps xs [] = xs


writeConcatListFile :: FilePath -> [FilePath] -> IO ()
writeConcatListFile path files = writeUtf8File path $ T.unlines [ "file '" <> T.pack fp <> "'" | fp <- files ]


runProcChecked :: FilePath -> [String] -> IO ()
runProcChecked bin args = do
  (ec, out, err) <- readProcessWithExitCode bin args ""
  case ec of
    ExitSuccess -> pure ()
    ExitFailure _ -> throwIO . userError $ "Process failed: " <> bin <> "\n"
                <> unlines args <> "\nstdout:\n" <> out <> "\nstderr:\n" <> err


writeUtf8File :: FilePath -> Text -> IO ()
writeUtf8File path = Bs.writeFile path . Te.encodeUtf8

--------------------------------------------------------------------------------
-- AI server client


copyRemoteAssetIntoLocalStore
  :: At.S3Conn
  -> Pool
  -> AiClient
  -> UUID
  -> Text
  -> Text
  -> IO At.AssetRef
copyRemoteAssetIntoLocalStore s3Conn pool ai remoteAssetEid localName notes = do
  req0 <- Hc.parseRequest $ ai.baseUrl <> "/asset/" <> Uu.toString remoteAssetEid
  let
    req = req0 { Hc.requestHeaders = Ai.bearerHeaders ai.jwt }
  resp <- Hc.httpLbs req ai.manager
  putStrLn $ "@[copyRemoteAssetIntoLocalStore] fetche asset: " <> Uu.toString remoteAssetEid

  let
    contentType = maybe "application/octet-stream" Te.decodeUtf8 (lookup "Content-Type" resp.responseHeaders)
  putStrLn $ "@[copyRemoteAssetIntoLocalStore] inserting as: " <> T.unpack localName
  As.insertBytesAsAsset pool s3Conn localName contentType notes (Lbs.toStrict resp.responseBody)

