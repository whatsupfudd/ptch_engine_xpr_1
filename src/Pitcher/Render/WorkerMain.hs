{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Pitcher.Render.WorkerMain
  ( WorkerLoopCfg(..)
  , LeaseRecycleMode(..)
  , WorkerLoopStats(..)
  , defaultWorkerLoopCfg
  , runWorkerLoop
  , runWorkerLoopUntil
  , recycleExpiredLeasesGlobal
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (unless, when)
import Control.Concurrent (threadDelay)

import Data.Int (Int64, Int32)
import Data.Text (Text)
import qualified Data.Text as T

import System.Random (randomRIO)

import Hasql.Pool (Pool)
import qualified Hasql.Pool as Hp
import Hasql.Session (Session, statement)
import Hasql.Statement (Statement)
import qualified Hasql.TH as TH

import Pitcher.Render.TaskRunner
  ( TaskRunnerEnv (..)
  , runLeasedNodeToCompletion
  )
import Pitcher.Render.WorkerLease
  ( WorkerCaps (..)
  , LeasedNode (..)
  , leaseNextNode
  )

import DB.Helpers (runSessionOrThrow)

--------------------------------------------------------------------------------
-- Loop configuration

data LeaseRecycleMode
  = NoLeaseRecycle
  | RecycleExpiredGlobally { everyIdlePolls :: Int }
  deriving (Eq, Show)

data WorkerLoopCfg = WorkerLoopCfg
  { stopCheck :: IO Bool
  , idleBaseMicros :: Int
  , idleMaxMicros :: Int
  , failureSleepMicros :: Int
  , recycleMode :: LeaseRecycleMode
  , logMsg :: Text -> IO ()
  }
  deriving ()

data WorkerLoopStats = WorkerLoopStats
  { leasesTaken :: Int64
  , nodesCompleted :: Int64
  , nodesFailed :: Int64
  , idlePolls :: Int64
  , recyclePasses :: Int64
  , recycledLeases :: Int64
  }
  deriving (Eq, Show)

defaultWorkerLoopCfg :: WorkerLoopCfg
defaultWorkerLoopCfg =
  WorkerLoopCfg
    { stopCheck = pure False
    , idleBaseMicros = 500000
    , idleMaxMicros = 10000000
    , failureSleepMicros = 1500000
    , recycleMode = NoLeaseRecycle
    , logMsg = \_ -> pure ()
    }

emptyStats :: WorkerLoopStats
emptyStats =
  WorkerLoopStats
    { leasesTaken = 0
    , nodesCompleted = 0
    , nodesFailed = 0
    , idlePolls = 0
    , recyclePasses = 0
    , recycledLeases = 0
    }

--------------------------------------------------------------------------------
-- Public entry points

runWorkerLoop :: TaskRunnerEnv -> WorkerCaps -> WorkerLoopCfg -> IO WorkerLoopStats
runWorkerLoop env caps cfg = runWorkerLoopUntil env caps cfg cfg.stopCheck

runWorkerLoopUntil
  :: TaskRunnerEnv
  -> WorkerCaps
  -> WorkerLoopCfg
  -> IO Bool
  -> IO WorkerLoopStats
runWorkerLoopUntil env caps cfg shouldStop =
  loopPass 0 0 emptyStats
  where
    loopPass :: Int -> Int -> WorkerLoopStats -> IO WorkerLoopStats
    loopPass idleStreak idleSinceRecycle stats = do
      stopNow <- shouldStop
      if stopNow then do
          cfg.logMsg $
            "worker[" <> caps.owner <> "] stopping"
          pure stats
      else do
        leaseRes <- try $ leaseNextNode env.pool caps :: IO (Either SomeException (Maybe LeasedNode))
        case leaseRes of
          Left ex -> do
            cfg.logMsg $ "worker[" <> caps.owner <> "] lease error: " <> T.pack (show ex)
            threadDelay cfg.failureSleepMicros
            loopPass idleStreak idleSinceRecycle stats

          Right Nothing -> do
            let
              idleStreak' = idleStreak + 1
              idleSinceRecycle' = idleSinceRecycle + 1
              stats1 = stats { idlePolls = stats.idlePolls + 1 }
            (stats2, idleSinceRecycle'') <- maybeRecycle env.pool caps cfg idleSinceRecycle' stats1
            sleepMicros <- computeIdleSleep cfg idleStreak'
            cfg.logMsg $ "worker[" <> caps.owner <> "] idle; sleeping " <> tshow sleepMicros <> "us"
            threadDelay sleepMicros
            loopPass idleStreak' idleSinceRecycle'' stats2

          Right (Just nodeAny) -> do
            let
              node = nodeAny
              stats1 = stats { leasesTaken = stats.leasesTaken + 1 }
            cfg.logMsg $ "worker[" <> caps.owner <> "] leased node " <> (T.pack . show) node.nodeUid <> " exec=" <> node.exec
            runRes <- try $ runLeasedNodeToCompletion env caps node :: IO (Either SomeException Bool)
            case runRes of
              Left ex -> do
                cfg.logMsg $ "worker[" <> caps.owner <> "] runner exception on " <> (T.pack . show) node.nodeUid <> ": " <> T.pack (show ex)
                threadDelay cfg.failureSleepMicros
                loopPass 0 0 stats1
              Right ok -> do
                let
                  stats2 = if ok then
                        stats1 { nodesCompleted = stats1.nodesCompleted + 1 }
                      else
                        stats1 { nodesFailed = stats1.nodesFailed + 1 }
                cfg.logMsg $ "worker[" <> caps.owner <> "] finished node " <> (T.pack . show) node.nodeUid <> " ok=" <> boolText ok
                loopPass 0 0 stats2

--------------------------------------------------------------------------------
-- Recycling policy

maybeRecycle
  :: Pool
  -> WorkerCaps
  -> WorkerLoopCfg
  -> Int
  -> WorkerLoopStats
  -> IO (WorkerLoopStats, Int)
maybeRecycle pool caps cfg idleSinceRecycle stats =
  case cfg.recycleMode of
    NoLeaseRecycle ->
      pure (stats, idleSinceRecycle)

    RecycleExpiredGlobally everyN ->
      if idleSinceRecycle < max 1 everyN
        then pure (stats, idleSinceRecycle)
        else do
          recycled <- recycleExpiredLeasesGlobal pool
          when (recycled > 0) $
            cfg.logMsg $
              "worker[" <> caps.owner <> "] recycled expired leases: "
                <> tshow recycled

          pure
            ( stats
                { recyclePasses = stats.recyclePasses + 1
                , recycledLeases = stats.recycledLeases + recycled
                }
            , 0
            )

--------------------------------------------------------------------------------
-- Sleep policy

computeIdleSleep :: WorkerLoopCfg -> Int -> IO Int
computeIdleSleep cfg idleStreak = do
  let shiftAmt = min 20 (max 0 (idleStreak - 1))
      base = cfg.idleBaseMicros
      raw =
        if shiftAmt == 0
          then base
          else min cfg.idleMaxMicros (base * (2 ^ shiftAmt))

      jitterLo = max 0 (raw * 85 `div` 100)
      jitterHi = max jitterLo (raw * 115 `div` 100)

  randomRIO (jitterLo, min cfg.idleMaxMicros jitterHi)

boolText :: Bool -> Text
boolText True = "true"
boolText False = "false"

tshow :: Show a => a -> Text
tshow = T.pack . show

--------------------------------------------------------------------------------
-- Global stale-lease recycling
--
-- This is useful for the current two-tier design when no separate producer/manager
-- ticker is reclaiming abandoned leases. Later, in the three-tier design, you can
-- switch the worker config to NoLeaseRecycle and let managers own this logic.

recycleExpiredLeasesGlobal :: Pool -> IO Int64
recycleExpiredLeasesGlobal pool =
  runSessionOrThrow "recycleExpiredLeasesGlobalStmt" pool $ statement () recycleExpiredLeasesGlobalStmt


recycleExpiredLeasesGlobalStmt :: Statement () Int64
recycleExpiredLeasesGlobalStmt =
  [TH.rowsAffectedStatement|
    update prod.render_node n
    set status =
          case
            when n.attempt_count >= n.max_attempts
              then 'failed'
            else 'ready'
          end,
        lease_owner = null,
        lease_expires_at = null,
        error_text =
          case
            when n.attempt_count >= n.max_attempts then
              coalesce(n.error_text, '') ||
              case
                when coalesce(n.error_text, '') = '' then ''
                else '\n'
              end ||
              'Lease expired after final allowed attempt.'
            else n.error_text
          end,
        updated_at = now()
    where n.status in ('leased', 'running')
      and n.lease_expires_at is not null
      and n.lease_expires_at < now()
  |]