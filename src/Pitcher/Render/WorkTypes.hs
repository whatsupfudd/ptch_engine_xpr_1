{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Pitcher.Render.WorkTypes where

import Data.Int (Int32, Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as Uu
import Data.Time.Clock (UTCTime)

import qualified Data.Aeson as Ae
import GHC.Generics (Generic)

import Utils (sanitizeKey)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)


data WorkerCaps = WorkerCaps
  { owner :: Text
  , lane :: Text
  , execFilter :: Maybe Text
  , leaseSeconds :: Int32
  }


data ExecRequirements = ExecRequirements
  { lane :: Text
  , needsGpu :: Bool
  , minVramMb :: Maybe Int32
  , maxRuntimeSec :: Int32
  , scratchMb :: Int32
  }
  deriving (Eq, Show, Generic, Ae.FromJSON, Ae.ToJSON)

data LeasedNode = LeasedNode
  { renderJobUid :: Int64
  , narrationUid :: Int64
  , nodeUid :: Int64
  , deriveKey :: Text
  , lane :: Text
  , exec :: Text
  , ord :: Int32
  , sourceKind :: Maybe Text
  , sourceEid :: Maybe UUID
  , params :: Ae.Value
  , artifactKind :: Text
  , attemptCount :: Int32
  , maxAttempts :: Int32
  , leaseOwner :: Text
  , leaseExpiresAt :: UTCTime
  }
  deriving (Eq, Show)

data RenderInput = RenderInput
  { ord :: Int32
  , inputKind :: Text
  , refKind :: Text
  , refEid :: Maybe UUID
  , refDeriveKey :: Maybe Text
  , role :: Maybe Text
  }

data UpstreamAsset = UpstreamAsset
  { assetUid :: Int64
  , assetEid :: UUID
  , artifactKind :: Text
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
assetNameForNode node ext =
  sanitizeAssetName node.exec
    <> "-"
    <> sanitizeAssetName node.deriveKey
    <> "."
    <> ext

sanitizeAssetName :: Text -> Text
sanitizeAssetName =
  T.map repl
  where
    repl c
      | isAsciiLower c = c
      | isAsciiUpper c = c
      | isDigit c = c
      | otherwise = '_'

