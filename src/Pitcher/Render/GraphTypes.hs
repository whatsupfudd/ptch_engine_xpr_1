{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
module Pitcher.Render.GraphTypes where

import Data.Int (Int32, Int64)
import Data.Text (Text)

import GHC.Generics (Generic)

import qualified Data.Aeson as Ae

import Pitcher.NarrationTypes (DialogueRender (..), VisualRender (..))
import Utils (tshow)

data RenderGraph = RenderGraph
  { schemaVer :: Int32
  , narrationUid :: Int64
  , nodes :: [RenderNodeSpec]
  , edges :: [RenderEdgeSpec]
  }
  deriving (Eq, Generic)

instance Show RenderGraph where
  show graph = "RenderGraph { schemaVer = " <> show graph.schemaVer 
        <> "\n, narrationUid = " <> show graph.narrationUid
        <> "\n, nodes = " <> show graph.nodes
        <> "\n, edges = " <> show graph.edges <> "\n }"


data NodeStage =
    AudioStage
  | ImageStage
  | SegmentStage
  | FinalStage
  | BlenderStage
  deriving (Eq, Ord, Show, Generic, Ae.ToJSON)


data NodeExec =
    AiTextToSpeechExec
  | AiTextToImageExec
  | FfmpegSegmentExec
  | FfmpegConcatExec
  | BlenderExec
  deriving (Eq, Ord, Show, Generic, Ae.ToJSON)


data NodeStatus =
    PendingNS
  | ReadyNS
  | LeasedNS
  | RunningNS
  | DoneNS
  | FailedNS
  | SkippedNS
  deriving (Eq, Ord, Show, Generic)

data ExecRequirements = ExecRequirements
  { lane :: Text
  , needsGpu :: Bool
  , minVramMb :: Maybe Int32
  , maxRuntimeSec :: Int32
  , scratchMb :: Int32
  }
  deriving (Eq, Show, Generic, Ae.ToJSON)

data ExpectedArtifact = ExpectedArtifact
  { kind :: Text
  , fileName :: Text
  , contentType :: Text
  }
  deriving (Eq, Show, Generic, Ae.ToJSON)

data RenderNodeSpec = RenderNodeSpec
  { key :: Text
  , stage :: NodeStage
  , exec :: NodeExec
  , ord :: Int32
  , dialogueFk :: Maybe Int64
  , visualOrd :: Maybe Int32
  , sourceSig :: Text
  , payload :: Ae.Value
  , requirements :: ExecRequirements
  , outputs :: [ExpectedArtifact]
  , maxAttempts :: Int32
  }
  deriving (Eq, Generic, Ae.ToJSON)

instance Show RenderNodeSpec where
  show node = "RenderNodeSpec { key = " <> show node.key
        <> "\n, stage = " <> show node.stage
        <> "\n, exec = " <> show node.exec
        <> "\n, ord = " <> show node.ord
        <> "\n, dialogueFk = " <> show node.dialogueFk
        <> "\n, visualOrd = " <> show node.visualOrd
        <> "\n, sourceSig = " <> show node.sourceSig
        <> "\n, payload = " <> show node.payload
        <> "\n, requirements = " <> show node.requirements
        <> "\n, outputs = " <> show node.outputs
        <> "\n, maxAttempts = " <> show node.maxAttempts <> "\n }"


data RenderEdgeSpec = RenderEdgeSpec
  { fromKey :: Text
  , toKey :: Text
  }
  deriving (Eq, Generic)

instance Show RenderEdgeSpec where
  show edge = "RenderEdgeSpec { " <> show edge.fromKey <> " -> " <> show edge.toKey <> " }"


audioNodeKey :: DialogueRender -> Text
audioNodeKey dlg =
  "audio:" <> tshow dlg.uid

imageNodeKey :: DialogueRender -> VisualRender -> Text
imageNodeKey dlg vis =
  "image:" <> tshow dlg.uid <> ":" <> tshow vis.ord

segmentNodeKey :: DialogueRender -> Text
segmentNodeKey dlg =
  "segment:" <> tshow dlg.uid

finalNodeKey :: Text
finalNodeKey = "final"


stageText :: NodeStage -> Text
stageText = \case
  AudioStage -> "audio"
  ImageStage -> "image"
  SegmentStage -> "segment"
  FinalStage -> "final"
  BlenderStage -> "blender"

nodeExecToText :: NodeExec -> Text
nodeExecToText = \case
  AiTextToSpeechExec -> "ai_tts"
  AiTextToImageExec -> "ai_image"
  FfmpegSegmentExec -> "ffmpeg_segment"
  FfmpegConcatExec -> "ffmpeg_concat"
  BlenderExec -> "blender"
  _ -> "unknown"


textToNodeExec :: Text -> Maybe NodeExec
textToNodeExec = \case
  "ai_tts" -> Just AiTextToSpeechExec
  "ai_image" -> Just AiTextToImageExec
  "ffmpeg_segment" -> Just FfmpegSegmentExec
  "ffmpeg_concat" -> Just FfmpegConcatExec
  "blender" -> Just BlenderExec
  _ -> Nothing