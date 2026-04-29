{-# LANGUAGE DeriveGeneric #-}
module Pitcher.NarrationTypes where

import Data.Int (Int32, Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)

import GHC.Generics (Generic)

-- DB items:
data NarrationRender = NarrationRender
  { narrationUid :: Int64
  , eid :: UUID
  , dialogues :: [DialogueRender]
  }
  deriving (Eq, Show, Generic)


data DialogueRender = DialogueRender
  { uid :: Int64
  , eid :: UUID
  , ord :: Int32
  , emotion :: Text
  , sentences :: [Text]
  , visuals :: [VisualRender]
  }
  deriving (Eq, Show, Generic)

data VisualRender = VisualRender
  { uid :: Int64
  , eid :: UUID
  , ord :: Int32
  , sentenceIx :: Maybe Int32
  , description :: Text
  }
  deriving (Eq, Show, Generic)


dialogueSpokenText :: DialogueRender -> Text
dialogueSpokenText dlg =  T.intercalate " " dlg.sentences

-- Ingest items:
newtype Narration = Narration {
    dialogues :: [Dialogue]
  }
  deriving (Eq, Show)

data Dialogue = Dialogue
  { emotions :: [Text]
  , sentences :: [Text]
  , visuals :: [DialogueVisual]
  }
  deriving (Eq, Show)


data DialogueVisual = DialogueVisual
  { sentenceOrd :: Maybe Int32
  , description :: Text
  }
  deriving (Eq, Show)


data RawItem =
    RawContent Text
  | RawVisual DialogueVisual
  deriving (Eq, Show)
