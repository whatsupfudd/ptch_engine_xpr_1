{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pitcher.Render.Producer
  ( ProducerCfg(..)
  , ProducerTick(..)
  , RenderGraph(..)
  , RenderNodeSpec(..)
  , RenderEdgeSpec(..)
  , NodeStage(..)
  , NodeExec(..)
  , NodeStatus(..)
  , ExecRequirements(..)
  , ExpectedArtifact(..)
  , NarrationRender(..)
  , DialogueRender(..)
  , VisualRender(..)
  , launchProducer
  , producerTick
  , buildRenderGraph
  ) where

import Control.Exception (throwIO)
import Control.Monad (forM, forM_, when, unless)
import Control.Monad.Except (throwError, MonadError (catchError))
import Control.Monad.Error.Class (MonadError)

import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as Lbs
import Data.Int (Int32, Int64)
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Te
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import qualified Data.Vector as Vc

import GHC.Generics (Generic)

import qualified Data.Aeson as Ae
import Data.Aeson ((.=))

import Hasql.Pool (Pool)
import qualified Hasql.Pool as Hp
import qualified Hasql.Transaction as HT
import Hasql.Session (Session, statement)

import Pitcher.Render.Types
import Pitcher.NarrationTypes (DialogueRender (..), VisualRender (..), NarrationRender (..), dialogueSpokenText)
import Pitcher.Render.GraphTypes
import DB.Helpers (runSessionOrThrow, runTx)
import qualified DB.ProducerStmt as Ps
import qualified DB.LaunchOps as Lo
import Utils (sigText, tshow)


--------------------------------------------------------------------------------
-- Entry points

launchProducer :: ProducerCfg -> Pool -> UUID -> IO Int64
launchProducer cfg pool narrationEid = do
  mbUid <- runSessionOrThrow pool $ statement narrationEid Ps.selectNarrationUidStmt
  case mbUid of
    Nothing -> throwIO . userError $ "@[launchProducer] narration not found."
    Just narrationUid -> do
      narration <- Lo.loadNarrationRender pool narrationUid
      when (null narration.dialogues) $
        throwIO . userError $ "@[launchProducer] narration has no dialogues."

      jobUid <- Lo.loadOrCreateRenderJob pool narrationUid
      let
        graph = buildRenderGraph cfg narration
      putStrLn $ "@[launchProducer] graph: " <> show graph
      graphUid <- persistGraph pool jobUid graph
      _ <- producerTick cfg pool jobUid
      pure graphUid


--------------------------------------------------------------------------------
-- Pure graph builder

buildRenderGraph :: ProducerCfg -> NarrationRender -> RenderGraph
buildRenderGraph cfg narration =
  let 
    audioNodes = [ mkAudioNode cfg dlg | dlg <- narration.dialogues ]

    imageNodes = [ mkImageNode cfg dlg vis | dlg <- narration.dialogues, vis <- dlg.visuals ]
    segmentNodes = [ mkSegmentNode cfg dlg | dlg <- narration.dialogues ]
    finalNode = mkFinalNode cfg narration

    edges =
      [ RenderEdgeSpec (audioNodeKey dlg) (segmentNodeKey dlg) | dlg <- narration.dialogues ]
      <> [ RenderEdgeSpec (imageNodeKey dlg vis) (segmentNodeKey dlg) | dlg <- narration.dialogues, vis <- dlg.visuals ]
      <> [ RenderEdgeSpec (segmentNodeKey dlg) finalNodeKey | dlg <- narration.dialogues ]
  in
  RenderGraph {
      schemaVer = cfg.graphSchemaVer
    , narrationUid = narration.narrationUid
    , nodes = audioNodes <> imageNodes <> segmentNodes <> [finalNode]
    , edges = edges
    }

mkAudioNode :: ProducerCfg -> DialogueRender -> RenderNodeSpec
mkAudioNode cfg dlg =
  let
    out = ExpectedArtifact {
            kind = "audio"
          , fileName = "dialogue_" <> tshow dlg.ord <> ".mp3"
          , contentType = "audio/mpeg"
          }
  in
  RenderNodeSpec {
      key = audioNodeKey dlg
    , stage = AudioStage
    , exec = AiTextToSpeechExec
    , ord = dlg.ord
    , dialogueFk = Just dlg.uid
    , visualOrd = Nothing
    , sourceSig = sigText $
        Ae.object
          [ "v" .= cfg.renderVersionTag
          , "stage" .= ("audio" :: Text)
          , "dialogueUid" .= dlg.uid
          , "emotion" .= dlg.emotion
          , "spokenText" .= dialogueSpokenText dlg
          ]
    , payload =
        Ae.object
          [ "task" .= ("audio" :: Text)
          , "dialogueUid" .= dlg.uid
          , "emotion" .= dlg.emotion
          , "spokenText" .= dialogueSpokenText dlg
          ]
    , requirements =
        ExecRequirements
          { lane = "ai"
          , needsGpu = False
          , minVramMb = Nothing
          , maxRuntimeSec = 180
          , scratchMb = 128
          }
    , outputs = [out]
    , maxAttempts = cfg.defaultMaxAttempts
    }


mkImageNode :: ProducerCfg -> DialogueRender -> VisualRender -> RenderNodeSpec
mkImageNode cfg dlg vis =
  let
    out = ExpectedArtifact {
        kind = "image"
      , fileName = "visual_" <> tshow dlg.ord <> "_" <> tshow vis.ord <> ".png"
      , contentType = "image/png"
      }
  in
  RenderNodeSpec {
      key = imageNodeKey dlg vis
    , stage = ImageStage
    , exec = AiTextToImageExec
    , ord = dlg.ord * 1000 + vis.ord
    , dialogueFk = Just dlg.uid
    , visualOrd = Just vis.ord
    , sourceSig = sigText $
        Ae.object
          [ "v" .= cfg.renderVersionTag
          , "stage" .= ("image" :: Text)
          , "dialogueUid" .= dlg.uid
          , "visualOrd" .= vis.ord
          , "sentenceIx" .= vis.sentenceIx
          , "description" .= vis.description
          ]
    , payload =
        Ae.object
          [ "task" .= ("image" :: Text)
          , "dialogueUid" .= dlg.uid
          , "visualOrd" .= vis.ord
          , "sentenceIx" .= vis.sentenceIx
          , "description" .= vis.description
          ]
    , requirements =
        ExecRequirements
          { lane = "ai"
          , needsGpu = False
          , minVramMb = Nothing
          , maxRuntimeSec = 300
          , scratchMb = 256
          }
    , outputs = [out]
    , maxAttempts = cfg.defaultMaxAttempts
    }

mkSegmentNode :: ProducerCfg -> DialogueRender -> RenderNodeSpec
mkSegmentNode cfg dlg =
  let
    out = ExpectedArtifact {
            kind = "segment"
          , fileName = "segment_" <> tshow dlg.ord <> ".mp4"
          , contentType = "video/mp4"
          }
    audioSig = (mkAudioNode cfg dlg).sourceSig
    imageSigs = [ (mkImageNode cfg dlg vis).sourceSig | vis <- dlg.visuals ]
  in
  RenderNodeSpec {
      key = segmentNodeKey dlg
    , stage = SegmentStage
    , exec = FfmpegSegmentExec
    , ord = dlg.ord
    , dialogueFk = Just dlg.uid
    , visualOrd = Nothing
    , sourceSig = sigText $
        Ae.object
          [ "v" .= cfg.renderVersionTag
          , "stage" .= ("segment" :: Text)
          , "dialogueUid" .= dlg.uid
          , "audioSig" .= audioSig
          , "imageSigs" .= imageSigs
          ]
    , payload =
        Ae.object
          [ "task" .= ("segment" :: Text)
          , "dialogueUid" .= dlg.uid
          , "audioNodeKey" .= audioNodeKey dlg
          , "imageNodeKeys" .= [ imageNodeKey dlg vis | vis <- dlg.visuals ]
          , "spokenText" .= dialogueSpokenText dlg
          ]
    , requirements =
        ExecRequirements
          { lane = "video"
          , needsGpu = True
          , minVramMb = Just 2048
          , maxRuntimeSec = 1200
          , scratchMb = 4096
          }
    , outputs = [out]
    , maxAttempts = cfg.defaultMaxAttempts
    }

mkFinalNode :: ProducerCfg -> NarrationRender -> RenderNodeSpec
mkFinalNode cfg narration =
  let
    out = ExpectedArtifact
          { kind = "final"
          , fileName = "narration_" <> tshow narration.narrationUid <> ".mp4"
          , contentType = "video/mp4"
          }
    segmentSigs = [ (mkSegmentNode cfg dlg).sourceSig | dlg <- narration.dialogues ]
  in
  RenderNodeSpec {
        key = finalNodeKey
      , stage = FinalStage
      , exec = FfmpegConcatExec
      , ord = 1000000000
      , dialogueFk = Nothing
      , visualOrd = Nothing
      , sourceSig = sigText $
          Ae.object
            [ "v" .= cfg.renderVersionTag
            , "stage" .= ("final" :: Text)
            , "segmentSigs" .= segmentSigs
            , "gapSeconds" .= cfg.finalGapSeconds
            , "fadeSeconds" .= cfg.finalFadeSeconds
            ]
      , payload =
          Ae.object
            [ "task" .= ("final" :: Text)
            , "segmentNodeKeys" .= [ segmentNodeKey dlg | dlg <- narration.dialogues ]
            , "gapSeconds" .= cfg.finalGapSeconds
            , "fadeSeconds" .= cfg.finalFadeSeconds
            ]
      , requirements =
          ExecRequirements
            { lane = "video"
            , needsGpu = False
            , minVramMb = Nothing
            , maxRuntimeSec = 1800
            , scratchMb = 8192
            }
      , outputs = [out]
      , maxAttempts = cfg.defaultMaxAttempts
      }


producerTick :: ProducerCfg -> Pool -> Int64 -> IO ProducerTick
producerTick cfg pool jobUid = do
  rez <- runTx pool $ producerTickTx cfg jobUid
  putStrLn $ "@[producerTick] tick: " <> show rez
  pure rez


instance MonadError Text HT.Transaction where
  throwError = fail . T.unpack
  catchError = catchError

instance MonadFail HT.Transaction where
  fail = throwError . T.pack


producerTickTx :: ProducerCfg -> Int64 -> HT.Transaction ProducerTick
producerTickTx cfg jobUid = do
  lockOk <- HT.statement jobUid Ps.tryAdvisoryJobLockStmt
  unless lockOk $
    fail $ "@[producerTick] already being advanced by another producer: " <> show jobUid

  graphUid <- HT.statement jobUid Ps.findGraphUidStmt >>= \case
    Nothing -> fail $ "@[producerTick] render graph not found for render job: " <> show jobUid
    Just uid -> pure uid

  -- , cfg.defaultLeaseSeconds
  recycled <- HT.statement graphUid Ps.recycleExpiredLeasesStmt
  reusable <- HT.statement graphUid Ps.markReusableNodesDoneStmt
  promoted <- HT.statement graphUid Ps.promoteReadyNodesStmt
  completed <- HT.statement jobUid Ps.finalizeGraphIfDoneStmt

  let
    tick = ProducerTick { 
        promotedReady = promoted
      , recycledExpired = recycled
        , markedReusable = reusable
        , graphCompleted = completed
        }
  pure tick

--------------------------------------------------------------------------------
-- Graph persistence

persistGraph :: Pool -> Int64 -> RenderGraph -> IO Int64
persistGraph pool jobUid graph =
  runTx pool $ persistGraphTx jobUid graph


persistGraphTx :: Int64 -> RenderGraph -> HT.Transaction Int64
persistGraphTx jobUid graph = do
  graphUid <- HT.statement (jobUid, graph.schemaVer) Ps.insertRenderGraphStmt

  nodeUidByKey <- fmap toNodeMap $
    forM graph.nodes $ \node ->
      let
        nodeParams = (
            graphUid
          , node.key
          , stageText node.stage
          , nodeExecToText node.exec
          , node.ord
          , node.dialogueFk
          , node.visualOrd
          , outputKind node.outputs
          , node.sourceSig
          , Ae.toJSON node.requirements
          , node.payload
          , node.maxAttempts
          )
      in do
      nodeUid <- HT.statement nodeParams Ps.insertRenderNodeStmt
      pure (node.key, nodeUid)

  HT.statement graphUid Ps.deleteRenderEdgesStmt

  forM_ graph.edges $ \edge -> do
    fromUid <- lookupNodeOrFail edge.fromKey nodeUidByKey
    toUid <- lookupNodeOrFail edge.toKey nodeUidByKey
    HT.statement (graphUid, fromUid, toUid) Ps.insertRenderEdgeStmt

  pure graphUid


outputKind :: [ExpectedArtifact] -> Maybe Text
outputKind outputs =
  case outputs of
    [] -> Nothing
    x : _ -> Just x.kind


toNodeMap :: [(Text, Int64)] -> [(Text, Int64)]
toNodeMap = id


lookupNodeOrFail :: Text -> [(Text, Int64)] -> HT.Transaction Int64
lookupNodeOrFail key pairs =
  case L.lookup key pairs of
    Nothing -> fail $ "@[lookupNodeOrFail] missing graph node key " <> T.unpack key
    Just uid -> pure uid
