{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Pitcher.Render.WorkerLease
  ( WorkerCaps(..)
  , LeasedNode(..)
  , CompleteSuccess(..)
  , CompleteFailure(..)
  , leaseNextNode
  , heartbeatNodeLease
  , completeNodeSuccess
  , completeNodeFailure
  , recycleExpiredLeases
  ) where

import Control.Exception (throwIO)

import Data.Bifunctor (first)
import Data.Int (Int32, Int64)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)

import GHC.Generics (Generic)

import qualified Data.Aeson as Ae

import Hasql.Pool (Pool)
import qualified Hasql.Transaction as HT

import qualified DB.LeaseStmt as Ls
import DB.Helpers (runSessionOrThrow, runTx)
import Pitcher.Render.WorkTypes


leaseNextNode :: Pool -> WorkerCaps -> IO (Maybe LeasedNode)
leaseNextNode pool caps =
  runTx pool $ do
    mbRow <- HT.statement
        (caps.owner, caps.lane, caps.hasGpu, caps.vramMb, caps.leaseSeconds)
        Ls.leaseNextNodeStmt
    pure (decodeLeasedNode <$> mbRow)


heartbeatNodeLease :: Pool -> Int64 -> WorkerCaps -> IO Bool
heartbeatNodeLease pool nodeUid caps =
  runTx pool $ do
    mbOk <-
      HT.statement
        (nodeUid, caps.owner, caps.leaseSeconds)
        Ls.heartbeatNodeLeaseStmt
    pure (isJust mbOk)

completeNodeSuccess :: Pool -> CompleteSuccess -> IO Bool
completeNodeSuccess pool input =
  runTx pool $ do
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
  runTx pool $ do
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


recycleExpiredLeases :: Pool -> Int64 -> IO Int64
recycleExpiredLeases pool graphUid =
  runTx pool $ HT.statement graphUid Ls.recycleExpiredLeasesStmt

--------------------------------------------------------------------------------
-- Decoding helpers

decodeLeasedNode :: Ls.LeaseNextNodeRaw -> LeasedNode
decodeLeasedNode
  ( jobUid
  , graphUid
  , nodeUid
  , key
  , stage
  , exec
  , ord
  , dialogueFk
  , visualOrd
  , artifactKind
  , sourceSig
  , requirementsJson
  , payload
  , attemptCount
  , maxAttempts
  , leaseOwner
  , leaseExpiresAt
  ) =
    LeasedNode
      { jobUid = jobUid
      , graphUid = graphUid
      , nodeUid = nodeUid
      , key = key
      , stage = stage
      , exec = exec
      , ord = ord
      , dialogueFk = dialogueFk
      , visualOrd = visualOrd
      , artifactKind = artifactKind
      , sourceSig = sourceSig
      , requirements = parseJsonOrCrash "ExecRequirements" requirementsJson
      , payload = payload
      , attemptCount = attemptCount
      , maxAttempts = maxAttempts
      , leaseOwner = leaseOwner
      , leaseExpiresAt = leaseExpiresAt
      }

parseJsonOrCrash :: Ae.FromJSON a => String -> Ae.Value -> a
parseJsonOrCrash label val =
  case Ae.fromJSON val of
    Ae.Error err ->
      error $
        label <> " decode failed: " <> err
    Ae.Success x ->
      x

fromMaybeFalse :: Maybe Bool -> Bool
fromMaybeFalse =
  maybe False id


