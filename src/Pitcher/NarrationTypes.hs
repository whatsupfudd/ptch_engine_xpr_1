{-# LANGUAGE DeriveGeneric #-}
module Pitcher.NarrationTypes where

import Data.Int (Int32, Int64)
import Data.Text (Text)
import qualified Data.Text as T

import GHC.Generics (Generic)


data DialogueRender = DialogueRender
  { uid :: Int64
  , ord :: Int32
  , emotion :: Text
  , sentences :: [Text]
  , visuals :: [VisualRender]
  }
  deriving (Eq, Show, Generic)

data VisualRender = VisualRender
  { ord :: Int32
  , sentenceIx :: Maybe Int32
  , description :: Text
  }
  deriving (Eq, Show, Generic)

data NarrationRender = NarrationRender
  { narrationUid :: Int64
  , dialogues :: [DialogueRender]
  }
  deriving (Eq, Show, Generic)

dialogueSpokenText :: DialogueRender -> Text
dialogueSpokenText dlg =  T.intercalate " " dlg.sentences

