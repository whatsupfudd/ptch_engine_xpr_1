{-# LANGUAGE DuplicateRecordFields #-}

module Pitcher.Render.WorkerLease
  ( WorkerCaps(..)
  , LeasedNode(..)
  , CompleteSuccess(..)
  , CompleteFailure(..)
  , RenderInput(..)
  , UpstreamAsset(..)

  , leaseNextNode
  , leaseNextNodeForExec
  , heartbeatNodeLease
  , completeNodeSuccess
  , completeNodeFailure
  , recycleExpiredLeases
  , recycleExpiredLeasesGlobal
  , loadNodeInputs
  , lookupUpstreamNodeAsset
  ) where

import Control.Monad (void)

import Data.Int (Int32, Int64)
import Data.Maybe (isJust, fromMaybe)
import Data.Text (Text, unpack)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)

import qualified Data.Aeson as Ae
import qualified Data.Vector as Vc

import Hasql.Pool (Pool)
import qualified Hasql.Transaction as HT

import qualified DB.LeaseStmt as Ls
import DB.Helpers (runTx)

import Pitcher.Render.WorkTypes
  ( CompleteFailure(..)
  , CompleteSuccess(..)
  , LeasedNode(..)
  , RenderInput(..)
  , UpstreamAsset(..)
  , WorkerCaps(..)
  )

--------------------------------------------------------------------------------
-- Leasing

leaseNextNode :: Pool -> WorkerCaps -> IO (Maybe LeasedNode)
leaseNextNode pool caps =
  runTx "leaseNextNode" pool $ do
    mbRow <- HT.statement ( caps.owner, caps.lane, caps.execFilter, caps.leaseSeconds) Ls.leaseNextNodeStmt
    pure (decodeLeasedNode <$> mbRow)

leaseNextNodeForExec :: Pool -> WorkerCaps -> Text -> IO (Maybe LeasedNode)
leaseNextNodeForExec pool caps execName =
  leaseNextNode pool caps { execFilter = Just execName }

--------------------------------------------------------------------------------
-- Heartbeat

heartbeatNodeLease :: Pool -> Int64 -> WorkerCaps -> IO Bool
heartbeatNodeLease pool nodeUid caps =
  runTx "heartbeatNodeLease" pool $ do
    mbOk <-
      HT.statement
        ( nodeUid
        , caps.owner
        , caps.leaseSeconds
        )
        Ls.heartbeatNodeLeaseStmt

    pure (isJust mbOk)

--------------------------------------------------------------------------------
-- Completion

completeNodeSuccess :: Pool -> CompleteSuccess -> IO Bool
completeNodeSuccess pool input =
  runTx "completeNodeSuccess" pool $ do
    mbOk <-
      HT.statement
        ( input.nodeUid
        , input.owner
        , input.assetUid
        , input.assetEid
        , input.requestEid
        , input.notes
        )
        Ls.completeNodeSuccessStmt

    pure (fromMaybeFalse mbOk)

completeNodeFailure :: Pool -> CompleteFailure -> IO Bool
completeNodeFailure pool input =
  runTx "completeNodeFailure" pool $ do
    mbOk <-
      HT.statement
        ( input.nodeUid
        , input.owner
        , input.retryable
        , input.errorText
        , input.notes
        , input.requestEid
        )
        Ls.completeNodeFailureStmt

    pure (fromMaybeFalse mbOk)

--------------------------------------------------------------------------------
-- Lease recycling

-- | Recycle stale leases for a single render job.
--
-- The argument is now render_job.uid, not graph.uid.
recycleExpiredLeases :: Pool -> Int64 -> IO Int64
recycleExpiredLeases pool renderJobUid =
  runTx "recycleExpiredLeases" pool $
    HT.statement renderJobUid Ls.recycleExpiredLeasesStmt

-- | Recycle stale leases globally.
--
-- Useful for standalone direct workers. In the later manager/executor design,
-- this should usually be owned by producer/manager ticker logic instead.
recycleExpiredLeasesGlobal :: Pool -> IO Int64
recycleExpiredLeasesGlobal pool =
  runTx "recycleExpiredLeasesGlobal" pool $
    HT.statement () Ls.recycleExpiredLeasesGlobalStmt

--------------------------------------------------------------------------------
-- Inputs and upstream assets

loadNodeInputs :: Pool -> Int64 -> IO [RenderInput]
loadNodeInputs pool nodeUid =
  runTx "loadNodeInputs" pool $ do
    rows <- HT.statement nodeUid Ls.selectNodeInputsStmt
    pure (map decodeRenderInput $ Vc.toList rows)

lookupUpstreamNodeAsset :: Pool -> Int64 -> Text -> IO (Maybe UpstreamAsset)
lookupUpstreamNodeAsset pool renderJobUid deriveKey = do
  rezA <- runTx "lookupUpstreamNodeAsset" pool $ do
    mbRow <- HT.statement (renderJobUid, deriveKey) Ls.selectUpstreamNodeAssetStmt
    pure (decodeUpstreamAsset <$> mbRow)
  case rezA of
    Just asset -> putStrLn ""
    Nothing -> putStrLn $ "@[lookupUpstreamNodeAsset] deriveKey not found: " <> unpack deriveKey
  pure rezA

--------------------------------------------------------------------------------
-- Decoding helpers

decodeLeasedNode :: Ls.LeaseNextNodeRaw -> LeasedNode
decodeLeasedNode
  ( renderJobUid
  , narrationUid
  , nodeUid
  , deriveKey
  , lane
  , exec
  , ord
  , sourceKind
  , sourceEid
  , params
  , artifactKind
  , attemptCount
  , maxAttempts
  , leaseOwner
  , leaseExpiresAt
  ) =
    LeasedNode
      { renderJobUid = renderJobUid
      , narrationUid = narrationUid
      , nodeUid = nodeUid
      , deriveKey = deriveKey
      , lane = lane
      , exec = exec
      , ord = ord
      , sourceKind = sourceKind
      , sourceEid = sourceEid
      , params = params
      , artifactKind = artifactKind
      , attemptCount = attemptCount
      , maxAttempts = maxAttempts
      , leaseOwner = leaseOwner
      , leaseExpiresAt = leaseExpiresAt
      }

decodeRenderInput :: Ls.RenderInputRaw -> RenderInput
decodeRenderInput
  ( ord
  , inputKind
  , refKind
  , refEid
  , refDeriveKey
  , role
  ) =
    RenderInput
      { ord = ord
      , inputKind = inputKind
      , refKind = refKind
      , refEid = refEid
      , refDeriveKey = refDeriveKey
      , role = role
      }

decodeUpstreamAsset :: (Int64, UUID, Text) -> UpstreamAsset
decodeUpstreamAsset
  ( assetUid
  , assetEid
  , artifactKind
  ) =
    UpstreamAsset
      { assetUid = assetUid
      , assetEid = assetEid
      , artifactKind = artifactKind
      }

fromMaybeFalse :: Maybe Bool -> Bool
fromMaybeFalse =
  fromMaybe False