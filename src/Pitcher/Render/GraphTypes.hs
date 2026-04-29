{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
module Pitcher.Render.GraphTypes where

import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.UUID (UUID)

import GHC.Generics (Generic)

import qualified Data.Aeson as Ae

import Pitcher.NarrationTypes (DialogueRender (..), VisualRender (..))
import Utils (tshow)

data NodeStage =
    AudioStage
  | ImageStage
  | SegmentStage
  | FinalStage
  | BlenderStage
  deriving (Eq, Ord, Show, Generic, Ae.ToJSON)


-- New:

data NodeLane
  = GenerateLane
  | FuseLane
  | FinalizeLane
  deriving (Eq, Ord, Show)

data NodeExec
  = AiTextToSpeechExec
  | AiTextToImageExec
  | FfmpegSegmentExec
  | FfmpegConcatExec
  | BlenderExec
  deriving (Eq, Ord, Show)

data SourceKind
  = NarrationSource
  | DialogueSource
  | VisualSource
  deriving (Eq, Ord, Show)

data InputKind
  = SourceInput
  | NodeInput
  deriving (Eq, Ord, Show)


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

-- New:


data NodeInputSpec = NodeInputSpec
  { ord :: Int32
  , inputKind :: InputKind
  , refKind :: Text
  , refEid :: UUID
  , role :: Maybe Text
  }
  deriving (Eq, Show)


data RenderNodeSpec = RenderNodeSpec
  { deriveKey :: Text
  , lane :: NodeLane
  , exec :: NodeExec
  , ord :: Int32
  , sourceKind :: Maybe SourceKind
  , sourceEid :: Maybe UUID
  , params :: Ae.Value
  , artifactKind :: Text
  , inputs :: [NodeInputSpec]
  , maxAttempts :: Int32
  }
  deriving (Eq, Show)


data RenderGraph = RenderGraph
  { narrationUid :: Int64
  , nodes :: [RenderNodeSpec]
  }
  deriving (Eq, Show)


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

--------------------------------------------------------------------------------
-- Text encoders

nodeLaneToText :: NodeLane -> Text
nodeLaneToText = \case
  GenerateLane -> "generate"
  FuseLane -> "fuse"
  FinalizeLane -> "finalize"


nodeExecToText :: NodeExec -> Text
nodeExecToText = \case
  AiTextToSpeechExec -> "ai_tts"
  AiTextToImageExec -> "ai_image"
  FfmpegSegmentExec -> "ffmpeg_segment"
  FfmpegConcatExec -> "ffmpeg_concat"
  BlenderExec -> "blender"

sourceKindToText :: SourceKind -> Text
sourceKindToText = \case
  NarrationSource -> "narration"
  DialogueSource -> "dialogue"
  VisualSource -> "visual"

inputKindToText :: InputKind -> Text
inputKindToText = \case
  SourceInput -> "source"
  NodeInput -> "node"

textToNodeExec :: Text -> Maybe NodeExec
textToNodeExec = \case
  "ai_tts" -> Just AiTextToSpeechExec
  "ai_image" -> Just AiTextToImageExec
  "ffmpeg_segment" -> Just FfmpegSegmentExec
  "ffmpeg_concat" -> Just FfmpegConcatExec
  "blender" -> Just BlenderExec
  _ -> Nothing