{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pitcher.Render.Producer
  ( ProducerCfg(..)
  , ProducerTick(..)

  , RenderGraph(..)
  , RenderNodeSpec(..)
  , RenderInputSpec(..)

  , NodeLane(..)
  , NodeExec(..)
  , SourceKind(..)
  , InputKind(..)

  , launchProducer
  , producerTick
  , buildRenderGraph

  , mkAudioNode
  , mkImageNode
  , mkSegmentNode
  , mkFinalNode
  ) where

import Control.Exception (throwIO)
import Control.Monad.Error.Class (MonadError, throwError, catchError)
import Control.Monad.Fail (MonadFail, fail)
import Control.Monad (forM_, unless, when)

import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as Uu

import qualified Data.Aeson as Ae
import Data.Aeson ((.=))

import Hasql.Pool (Pool)
import Hasql.Session (statement)
import qualified Hasql.Transaction as HT

import DB.Helpers ( runSessionOrThrow, runTx )
import qualified DB.ProducerStmt as Ps
import Pitcher.NarrationTypes ( DialogueRender(..), NarrationRender(..), VisualRender(..) )
import Pitcher.Render.GraphTypes ( NodeLane(..), NodeExec(..), SourceKind(..), InputKind(..), nodeLaneToText, nodeExecToText, sourceKindToText, inputKindToText )
import qualified DB.LaunchOps as Lo


--------------------------------------------------------------------------------
-- Producer config and tick report

data ProducerCfg = ProducerCfg
  { renderVersionTag :: Text
  , defaultMaxAttempts :: Int

  , ttsVoice :: Maybe Text
  , imageStyleTag :: Text

  , segmentPolicyTag :: Text
  , finalPolicyTag :: Text
  , finalGapSeconds :: Double
  , finalFadeSeconds :: Double
  }
  deriving (Eq, Show)

data ProducerTick = ProducerTick
  { promotedReady :: Int64
  , recycledExpired :: Int64
  , markedReusable :: Int64
  , graphCompleted :: Bool
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Simplified graph model

data RenderGraph = RenderGraph
  { narrationUid :: Int64
  , narrationEid :: UUID
  , nodes :: [RenderNodeSpec]
  }
  deriving (Eq, Show)

data RenderNodeSpec = RenderNodeSpec
  { deriveKey :: Text
  , lane :: NodeLane
  , exec :: NodeExec
  , ord :: Int

  , sourceKind :: Maybe SourceKind
  , sourceEid :: Maybe UUID

  , params :: Ae.Value
  , artifactKind :: Text
  , inputs :: [RenderInputSpec]

  , maxAttempts :: Int
  }
  deriving (Eq, Show)

data RenderInputSpec = RenderInputSpec
  { ord :: Int
  , inputKind :: InputKind
  , refKind :: Text
  , refEid :: Maybe UUID
  , refDeriveKey :: Maybe Text
  , role :: Maybe Text
  }
  deriving (Eq, Show)


instance MonadError Text HT.Transaction where
  throwError = fail . T.unpack
  catchError = catchError

instance MonadFail HT.Transaction where
  fail = throwError . T.pack



--------------------------------------------------------------------------------
-- Entry point

launchProducer :: ProducerCfg -> Pool -> UUID -> IO Int64
launchProducer cfg pool narrationEid = do
  narrationUid <-
    runSessionOrThrow "selectNarrationUidStmt" pool (statement narrationEid Ps.selectNarrationUidStmt) >>= \case
      Nothing ->
        throwIO . userError $
          "@[launchProducer] narration not found: " <> Uu.toString narrationEid
      Just uid ->
        pure uid

  narration <- Lo.loadNarrationRender pool (narrationUid, narrationEid)

  when (null narration.dialogues) $
    throwIO . userError $ "@[launchProducer] narration has no dialogues."

  jobUid <- runSessionOrThrow "createRenderJobStmt" pool $ statement narrationUid Ps.createRenderJobStmt
  _ <- runSessionOrThrow "markPreviousJobsSupersededStmt" pool $ statement jobUid Ps.markPreviousJobsSupersededStmt

  let graph = buildRenderGraph cfg narration
  persistGraph pool jobUid graph
  _ <- producerTick cfg pool jobUid
  pure jobUid


--------------------------------------------------------------------------------
-- Graph builder

buildRenderGraph :: ProducerCfg -> NarrationRender -> RenderGraph
buildRenderGraph cfg narration =
  let
    audioNodes =
      [ mkAudioNode cfg dlg
      | dlg <- narration.dialogues
      ]

    imageNodes =
      [ mkImageNode cfg dlg vis
      | dlg <- narration.dialogues
      , vis <- dlg.visuals
      ]

    segmentNodes =
      [ mkSegmentNode cfg dlg
      | dlg <- narration.dialogues
      ]

    finalNode =
      mkFinalNode cfg narration
  in
  RenderGraph
    { narrationUid = narration.narrationUid
    , narrationEid = narration.eid
    , nodes = audioNodes <> imageNodes <> segmentNodes <> [finalNode]
    }

--------------------------------------------------------------------------------
-- Node builders

mkAudioNode :: ProducerCfg -> DialogueRender -> RenderNodeSpec
mkAudioNode cfg dlg =
  let
    dkey =
      deriveKeyText
        [ "tts"
        , Uu.toText dlg.eid
        , fromMaybe "" cfg.ttsVoice
        , cfg.renderVersionTag
        ]

  in
  RenderNodeSpec
    { deriveKey = dkey
    , lane = GenerateLane
    , exec = AiTextToSpeechExec
    , ord = fromIntegral dlg.ord

    , sourceKind = Just DialogueSource
    , sourceEid = Just dlg.eid

    , params =
        Ae.object
          [ "voice" .= cfg.ttsVoice
          , "renderVersionTag" .= cfg.renderVersionTag
          ]

    , artifactKind = "audio"

    , inputs =
        [ sourceInput 1 "dialogue" dlg.eid (Just "dialogue")
        ]

    , maxAttempts = cfg.defaultMaxAttempts
    }

mkImageNode :: ProducerCfg -> DialogueRender -> VisualRender -> RenderNodeSpec
mkImageNode cfg _dlg vis =
  let
    dkey = deriveKeyText [ "image", Uu.toText vis.eid, cfg.imageStyleTag, cfg.renderVersionTag ]
  in
  RenderNodeSpec
    { deriveKey = dkey
    , lane = GenerateLane
    , exec = AiTextToImageExec
    , ord = fromIntegral vis.ord
    , sourceKind = Just VisualSource
    , sourceEid = Just vis.eid
    , params = Ae.object [ "imageStyleTag" .= cfg.imageStyleTag, "renderVersionTag" .= cfg.renderVersionTag ]
    , artifactKind = "image"
    , inputs = [ sourceInput 1 "visual" vis.eid (Just "visual") ]
    , maxAttempts = cfg.defaultMaxAttempts
    }

mkSegmentNode :: ProducerCfg -> DialogueRender -> RenderNodeSpec
mkSegmentNode cfg dlg =
  let
    audioNode =
      mkAudioNode cfg dlg

    imageNodes =
      [ mkImageNode cfg dlg vis
      | vis <- dlg.visuals
      ]

    imageTimingParts =
      [ Uu.toText vis.eid
          <> ":"
          <> maybe "" tshow vis.sentenceIx
      | vis <- dlg.visuals
      ]

    dkey =
      deriveKeyText $
        [ "segment"
        , audioNode.deriveKey
        , cfg.segmentPolicyTag
        , cfg.renderVersionTag
        ]
        <> map (.deriveKey) imageNodes
        <> imageTimingParts

    nodeInputs =
      [ nodeInput 1 audioNode.deriveKey (Just "audio")
      ]
      <> zipWith
          (\ix n -> nodeInput ix n.deriveKey (Just "image"))
          [2..]
          imageNodes

  in
  RenderNodeSpec
    { deriveKey = dkey
    , lane = FuseLane
    , exec = FfmpegSegmentExec
    , ord = fromIntegral dlg.ord

    , sourceKind = Just DialogueSource
    , sourceEid = Just dlg.eid

    , params =
        Ae.object
          [ "segmentPolicyTag" .= cfg.segmentPolicyTag
          , "renderVersionTag" .= cfg.renderVersionTag
          ]

    , artifactKind = "segment"

    , inputs = nodeInputs

    , maxAttempts = cfg.defaultMaxAttempts
    }

mkFinalNode :: ProducerCfg -> NarrationRender -> RenderNodeSpec
mkFinalNode cfg narration =
  let
    segmentNodes =
      [ mkSegmentNode cfg dlg
      | dlg <- narration.dialogues
      ]

    dkey =
      deriveKeyText $
        [ "final"
        , Uu.toText narration.eid
        , cfg.finalPolicyTag
        , cfg.renderVersionTag
        , tshow cfg.finalGapSeconds
        , tshow cfg.finalFadeSeconds
        ]
        <> map (.deriveKey) segmentNodes

    nodeInputs =
      zipWith
        (\ix n -> nodeInput ix n.deriveKey (Just "segment"))
        [1..]
        segmentNodes

  in
  RenderNodeSpec
    { deriveKey = dkey
    , lane = FinalizeLane
    , exec = FfmpegConcatExec
    , ord = 1000000000

    , sourceKind = Just NarrationSource
    , sourceEid = Just narration.eid

    , params =
        Ae.object
          [ "finalPolicyTag" .= cfg.finalPolicyTag
          , "gapSeconds" .= cfg.finalGapSeconds
          , "fadeSeconds" .= cfg.finalFadeSeconds
          , "renderVersionTag" .= cfg.renderVersionTag
          ]

    , artifactKind = "final"

    , inputs = nodeInputs

    , maxAttempts = cfg.defaultMaxAttempts
    }

--------------------------------------------------------------------------------
-- Producer tick

producerTick :: ProducerCfg -> Pool -> Int64 -> IO ProducerTick
producerTick cfg pool jobUid = do
  tick <- runTx "producerTickTx" pool $ producerTickTx cfg jobUid
  putStrLn $ "@[producerTick] tick: " <> show tick
  pure tick

producerTickTx :: ProducerCfg -> Int64 -> HT.Transaction ProducerTick
producerTickTx _cfg jobUid = do
  lockOk <- HT.statement (fromIntegral jobUid) Ps.tryAdvisoryJobLockStmt

  unless lockOk $ fail $
      "@[producerTick] render job already being advanced by another producer: " <> show jobUid

  recycled <- HT.statement jobUid Ps.recycleExpiredLeasesStmt
  reusable <- HT.statement jobUid Ps.markReusableNodesDoneStmt
  promoted <- HT.statement jobUid Ps.promoteReadyNodesStmt
  completed <- HT.statement jobUid Ps.finalizeRenderJobStmt

  pure
    ProducerTick
      { promotedReady = promoted
      , recycledExpired = recycled
      , markedReusable = reusable
      , graphCompleted = completed
      }

--------------------------------------------------------------------------------
-- Graph persistence

persistGraph :: Pool -> Int64 -> RenderGraph -> IO ()
persistGraph pool jobUid graph =
  runTx "persistGraphTx" pool $ persistGraphTx jobUid graph


persistGraphTx :: Int64 -> RenderGraph -> HT.Transaction ()
persistGraphTx jobUid graph =
  forM_ graph.nodes $ \node -> do
    nodeUid <-
      HT.statement
        ( jobUid
        , node.deriveKey
        , nodeLaneToText node.lane
        , nodeExecToText node.exec
        , fromIntegral node.ord
        , fmap sourceKindToText node.sourceKind
        , node.sourceEid
        , node.params
        , node.artifactKind
        , fromIntegral node.maxAttempts
        )
        Ps.insertRenderNodeStmt

    forM_ node.inputs $ \input ->
      HT.statement
        ( nodeUid
        , fromIntegral input.ord
        , inputKindToText input.inputKind
        , input.refKind
        , input.refEid
        , input.refDeriveKey
        , input.role
        )
        Ps.insertRenderInputStmt

--------------------------------------------------------------------------------
-- Input builders

sourceInput :: Int -> Text -> UUID -> Maybe Text -> RenderInputSpec
sourceInput ordVal refKind refEid role =
  RenderInputSpec
    { ord = ordVal
    , inputKind = SourceInput
    , refKind = refKind
    , refEid = Just refEid
    , refDeriveKey = Nothing
    , role = role
    }

nodeInput :: Int -> Text -> Maybe Text -> RenderInputSpec
nodeInput ordVal refDeriveKey role =
  RenderInputSpec
    { ord = ordVal
    , inputKind = NodeInput
    , refKind = "render_node"
    , refEid = Nothing
    , refDeriveKey = Just refDeriveKey
    , role = role
    }


--------------------------------------------------------------------------------
-- Derived-key helper
--
-- This is intentionally a single identity mechanism replacing the old
-- node-key/source-signature pair.
--
-- If you already have a SHA-256 or UUIDv5 helper in Utils, replace this
-- implementation with that helper. The important semantic property is that
-- the key is deterministic from stable source UUIDs and render-policy params.

deriveKeyText :: [Text] -> Text
deriveKeyText parts =
  "dk:" <> T.intercalate "\x1f" (map escapePart parts)

escapePart :: Text -> Text
escapePart =
  T.concatMap escapeChar
  where
    escapeChar '\x1f' = "\\x1f"
    escapeChar '\\' = "\\\\"
    escapeChar c = T.singleton c

tshow :: Show a => a -> Text
tshow =
  T.pack . show