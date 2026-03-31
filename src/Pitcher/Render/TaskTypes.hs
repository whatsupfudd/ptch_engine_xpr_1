{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Pitcher.Render.TaskTypes where

import Data.Int (Int64)
import Data.Map (Map)
import Data.Text (Text)
import Data.UUID (UUID)

import GHC.Generics (Generic)

import qualified Data.Aeson as Ae

import Utils (tshow)
import Pitcher.NarrationTypes (DialogueRender (..), VisualRender (..), NarrationRender (..))


-- V1:
data TaskStatus
  = PendingTS
  | QueuedTS
  | RunningTS
  | DoneTS
  | FailedTS
  | SkippedTS
  deriving (Eq, Show, Generic, Ae.ToJSON, Ae.FromJSON)

data TaskKind
  = AudioTK
  | ImageTK
  | SegmentTK
  | FinalTK
  deriving (Eq, Ord, Show, Generic, Ae.ToJSON, Ae.FromJSON)

data TaskSnapshot = TaskSnapshot
  { key :: Text
  , kind :: TaskKind
  , sourceSig :: Text
  , status :: TaskStatus
  , assetEid :: Maybe UUID
  , requestEid :: Maybe UUID
  , errorText :: Maybe Text
  }
  deriving (Eq, Show, Generic, Ae.ToJSON, Ae.FromJSON)

data PersistedRenderState = PersistedRenderState
  { narrationUid :: Int64
  , tasks :: [TaskSnapshot]
  , finalAssetEid :: Maybe UUID
  }
  deriving (Eq, Show, Generic, Ae.ToJSON, Ae.FromJSON)

-- V2:
data AudioTask = AudioTask
  { dialogue :: DialogueRender
  , sourceSig :: Text
  }
  deriving (Eq, Show)

data ImageTask = ImageTask
  { dialogue :: DialogueRender
  , visual :: VisualRender
  , sourceSig :: Text
  }
  deriving (Eq, Show)

data SegmentTask = SegmentTask
  { dialogue :: DialogueRender
  , audioKey :: Text
  , imageKeys :: [Text]
  , sourceSig :: Text
  }
  deriving (Eq, Show)

data FinalTask = FinalTask
  { segmentKeys :: [Text]
  , sourceSig :: Text
  }
  deriving (Eq, Show)

data RenderPlan = RenderPlan
  { narration :: NarrationRender
  , audioTasks :: Map Text AudioTask
  , imageTasks :: Map Text ImageTask
  , segmentTasks :: Map Text SegmentTask
  , finalTask :: FinalTask
  }
  deriving (Eq, Show)

audioTaskKey :: DialogueRender -> Text
audioTaskKey dlg = "audio:" <> tshow dlg.uid

imageTaskKey :: DialogueRender -> VisualRender -> Text
imageTaskKey dlg visual = "image:" <> tshow dlg.uid <> ":" <> tshow visual.ord

segmentTaskKey :: DialogueRender -> Text
segmentTaskKey dlg = "segment:" <> tshow dlg.uid

finalTaskKey :: Text
finalTaskKey = "final"


