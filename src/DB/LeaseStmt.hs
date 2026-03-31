{-# LANGUAGE QuasiQuotes #-}
module DB.LeaseStmt where

import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.UUID (UUID)
import Data.Time.Clock (UTCTime)

import qualified Data.Aeson as Ae

import qualified Hasql.TH as TH
import Hasql.Statement (Statement)
import qualified Hasql.Transaction as HT
import qualified Hasql.Transaction.Sessions as HTS


--------------------------------------------------------------------------------
-- SQL
--
-- Assumes:
--   prod.render_job
--   prod.render_graph
--   prod.render_node
--   prod.render_artifact
--
-- Important semantics:
--   * attempt_count increments when a node is leased
--   * lease expiry does NOT increment attempt_count again
--   * heartbeat turns leased -> running and extends the expiry
--   * success/failure require matching lease_owner and a live lease
--
-- Asset fk is bigint here, matching the later schema.

{-
jobUid
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
  )-}
type LeaseNextNodeRaw = (
          Int64
        , Int64
        , Int64
        , Text
        , Text
        , Text
        , Int32
        , Maybe Int64
        , Maybe Int32
        , Maybe Text
        , Text
        , Ae.Value
        , Ae.Value
        , Int32
        , Int32
        , Text
        , UTCTime
        )


-- Owner, lane, hasGpu, minVramMb, leaseSeconds.
leaseNextNodeStmt
  :: Statement
      (Text, Text, Bool, Maybe Int32, Int32)
      (Maybe LeaseNextNodeRaw)
leaseNextNodeStmt =
  [TH.maybeStatement|
    with candidate as (
      select
        n.uid,
        rg.render_job_fk
      from prod.render_node n
      join prod.render_graph rg on rg.uid = n.graph_fk
      where n.status = 'ready'
        and n.attempt_count < n.max_attempts
        and coalesce(n.requirements->>'lane', '') = $2::text
        and (
          coalesce((n.requirements->>'needsGpu')::bool, false) = false
          or $3::bool = true
        )
        and (
          coalesce((n.requirements->>'minVramMb')::int, 0) = 0
          or coalesce($4::int4?, 0) >= coalesce((n.requirements->>'minVramMb')::int, 0)
        )
      order by
        n.ord asc,
        n.uid asc
      limit 1
      for update skip locked
    )
    update prod.render_node n
    set status = 'leased',
        lease_owner = $1::text,
        lease_expires_at = now() + make_interval(secs => $5::int4),
        attempt_count = n.attempt_count + 1,
        updated_at = now(),
        error_text = null
    from candidate c
    where n.uid = c.uid
    returning
      c.render_job_fk::int8,
      n.graph_fk::int8,
      n.uid::int8,
      n.key::text,
      n.stage::text,
      n.exec::text,
      n.ord::int4,
      n.dialogue_fk::int8?,
      n.visual_ord::int4?,
      n.artifact_kind::text?,
      n.source_sig::text,
      n.requirements::jsonb,
      n.payload::jsonb,
      n.attempt_count::int4,
      n.max_attempts::int4,
      n.lease_owner::text,
      n.lease_expires_at::timestamptz
  |]

heartbeatNodeLeaseStmt :: Statement (Int64, Text, Int32) (Maybe Bool)
heartbeatNodeLeaseStmt =
  [TH.maybeStatement|
    update prod.render_node n
    set status =
          case
            when n.status = 'leased' then 'running'
            else n.status
          end,
        lease_expires_at = now() + make_interval(secs => $3::int4),
        updated_at = now()
    where n.uid = $1::int8
      and n.lease_owner = $2::text
      and n.status in ('leased', 'running')
      and n.lease_expires_at is not null
      and n.lease_expires_at >= now()
    returning true::bool
  |]

completeNodeSuccessStmt
  :: Statement
      (Int64, Text, Int64, UUID, Maybe UUID, Maybe Text)
      (Maybe Bool)
completeNodeSuccessStmt =
  [TH.maybeStatement|
    with updated as (
      update prod.render_node n
      set status = 'done',
          asset_fk = $3::int8,
          asset_eid = $4::uuid,
          lease_owner = null,
          lease_expires_at = null,
          completed_at = now(),
          error_text = null,
          updated_at = now()
      where n.uid = $1::int8
        and n.lease_owner = $2::text
        and n.status in ('leased', 'running')
        and n.lease_expires_at is not null
        and n.lease_expires_at >= now()
      returning
        n.graph_fk,
        n.artifact_kind,
        n.dialogue_fk,
        n.visual_ord,
        n.source_sig
    ),
    meta as (
      select
        rg.render_job_fk,
        u.artifact_kind,
        u.dialogue_fk,
        u.visual_ord,
        u.source_sig
      from updated u
      join prod.render_graph rg on rg.uid = u.graph_fk
    ),
    upsert_artifact as (
      insert into prod.render_artifact
        ( render_job_fk
        , kind
        , dialogue_fk
        , visual_ord
        , source_sig
        , status
        , asset_fk
        , asset_eid
        , request_eid
        , notes
        )
      select
        m.render_job_fk,
        m.artifact_kind,
        m.dialogue_fk,
        m.visual_ord,
        m.source_sig,
        'done',
        $3::int8,
        $4::uuid,
        $5::uuid?,
        $6::text?
      from meta m
      where m.artifact_kind is not null
      on conflict (render_job_fk, kind, dialogue_fk, visual_ord, source_sig)
      do update
        set status = 'done',
            asset_fk = excluded.asset_fk,
            asset_eid = excluded.asset_eid,
            request_eid = excluded.request_eid,
            notes = excluded.notes,
            updated_at = now()
      returning 1
    )
    select exists(select 1 from updated)::bool
  |]

completeNodeFailureStmt
  :: Statement
      (Int64, Text, Bool, Text, Maybe Text, Maybe UUID)
      (Maybe Bool)
completeNodeFailureStmt =
  [TH.maybeStatement|
    with updated as (
      update prod.render_node n
      set status =
            case
              when $3::bool = true and n.attempt_count < n.max_attempts
                then 'ready'
              else 'failed'
            end,
          lease_owner = null,
          lease_expires_at = null,
          error_text = $4::text,
          updated_at = now()
      where n.uid = $1::int8
        and n.lease_owner = $2::text
        and n.status in ('leased', 'running')
        and n.lease_expires_at is not null
        and n.lease_expires_at >= now()
      returning
        n.graph_fk,
        n.artifact_kind,
        n.dialogue_fk,
        n.visual_ord,
        n.source_sig,
        n.status
    ),
    meta as (
      select
        rg.render_job_fk,
        u.artifact_kind,
        u.dialogue_fk,
        u.visual_ord,
        u.source_sig,
        u.status
      from updated u
      join prod.render_graph rg on rg.uid = u.graph_fk
    ),
    upsert_terminal_artifact as (
      insert into prod.render_artifact
        ( render_job_fk
        , kind
        , dialogue_fk
        , visual_ord
        , source_sig
        , status
        , asset_fk
        , asset_eid
        , request_eid
        , notes
        )
      select
        m.render_job_fk,
        m.artifact_kind,
        m.dialogue_fk,
        m.visual_ord,
        m.source_sig,
        'failed',
        null,
        null,
        $6::uuid?,
        coalesce($5::text?, '') ||
          case
            when coalesce($5::text?, '') = '' then ''
            else '\n'
          end ||
          $4::text
      from meta m
      where m.artifact_kind is not null
        and m.status = 'failed'
      on conflict (render_job_fk, kind, dialogue_fk, visual_ord, source_sig)
      do update
        set status = 'failed',
            asset_fk = null,
            asset_eid = null,
            request_eid = excluded.request_eid,
            notes = excluded.notes,
            updated_at = now()
      returning 1
    )
    select exists(select 1 from updated)::bool
  |]

-- Corrected form:
-- since attempt_count increments on lease acquisition, lease expiry only
-- clears the stale lease and either returns the node to ready or marks it failed.
recycleExpiredLeasesStmt :: Statement Int64 Int64
recycleExpiredLeasesStmt =
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
    where n.graph_fk = $1::int8
      and n.status in ('leased', 'running')
      and n.lease_expires_at is not null
      and n.lease_expires_at < now()
  |]