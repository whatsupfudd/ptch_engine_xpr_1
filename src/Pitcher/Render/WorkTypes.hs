{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Pitcher.Render.WorkTypes where

import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.UUID (UUID)
import Data.Time.Clock (UTCTime)

import qualified Data.Aeson as Ae
import GHC.Generics (Generic)

import Utils (sanitizeKey)


data WorkerCaps = WorkerCaps
  { owner :: Text
  , lane :: Text
  , hasGpu :: Bool
  , vramMb :: Maybe Int32
  , leaseSeconds :: Int32
  }
  deriving (Eq, Show)

data ExecRequirements = ExecRequirements
  { lane :: Text
  , needsGpu :: Bool
  , minVramMb :: Maybe Int32
  , maxRuntimeSec :: Int32
  , scratchMb :: Int32
  }
  deriving (Eq, Show, Generic, Ae.FromJSON, Ae.ToJSON)

data LeasedNode = LeasedNode
  { jobUid :: Int64
  , graphUid :: Int64
  , nodeUid :: Int64
  , key :: Text
  , stage :: Text
  , exec :: Text
  , ord :: Int32
  , dialogueFk :: Maybe Int64
  , visualOrd :: Maybe Int32
  , artifactKind :: Maybe Text
  , sourceSig :: Text
  , requirements :: ExecRequirements
  , payload :: Ae.Value
  , attemptCount :: Int32
  , maxAttempts :: Int32
  , leaseOwner :: Text
  , leaseExpiresAt :: UTCTime
  }
  deriving (Eq, Show)

data CompleteSuccess = CompleteSuccess
  { nodeUid :: Int64
  , owner :: Text
  , assetUid :: Int64
  , assetEid :: UUID
  , requestEid :: Maybe UUID
  , notes :: Maybe Text
  }
  deriving (Eq, Show)

data CompleteFailure = CompleteFailure
  { nodeUid :: Int64
  , owner :: Text
  , retryable :: Bool
  , errorText :: Text
  , notes :: Maybe Text
  , requestEid :: Maybe UUID
  }
  deriving (Eq, Show)


assetNameForNode :: LeasedNode -> Text -> Text
assetNameForNode node ext = sanitizeKey node.key <> "." <> ext
