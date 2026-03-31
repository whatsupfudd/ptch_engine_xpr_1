module Pitcher.Render.Types where

import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.UUID (UUID)

import Assets.Types (S3Conn)
import AiSup.Types (AiRunnerCfg)

data RenderEnv = RenderEnv
  { aiCfg :: AiRunnerCfg
  , s3Conn :: S3Conn
  , ffmpegBin :: FilePath
  , ffprobeBin :: FilePath
  , widthPx :: Int
  , heightPx :: Int
  , fps :: Int
  , gapDurationSeconds :: Double
  , fadeDurationSeconds :: Double
  , renderVersionTag :: Text
  , failFast :: Bool
  , parallelism :: RenderParallelism
  }

data RenderParallelism = RenderParallelism
  { audioWorkers :: Int
  , imageWorkers :: Int
  , segmentWorkers :: Int
  }
  deriving (Eq, Show)

data RenderOutcome
  = RenderSucceeded
      { jobUid :: Int64
      , finalAssetEid :: UUID
      }
  | RenderFailed
      { jobUid :: Int64
      , reason :: Text
      }
  deriving (Eq, Show)

-- Producer Config:

data ProducerCfg = ProducerCfg
  { graphSchemaVer :: Int32
  , renderVersionTag :: Text
  , defaultMaxAttempts :: Int32
  , defaultLeaseSeconds :: Int32
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
