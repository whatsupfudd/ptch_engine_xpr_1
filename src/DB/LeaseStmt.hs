{-# LANGUAGE QuasiQuotes #-}

module DB.LeaseStmt
  ( LeaseNextNodeRaw
  , RenderInputRaw
  , leaseNextNodeStmt
  , leaseNextNodeCompatStmt
  , heartbeatNodeLeaseStmt
  , completeNodeSuccessStmt
  , completeNodeFailureStmt
  , recycleExpiredLeasesStmt
  , recycleExpiredLeasesGlobalStmt
  , selectNodeInputsStmt
  , selectUpstreamNodeAssetStmt
  ) where

import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Data.Vector (Vector)

import qualified Data.Aeson as Ae
import qualified Hasql.TH as TH
import Hasql.Statement (Statement)

--------------------------------------------------------------------------------
-- New leased-node shape
--
-- Corresponds to the simplified prod.render_node table.
--
--   renderJobUid
--   narrationUid
--   nodeUid
--   deriveKey
--   lane
--   exec
--   ord
--   sourceKind
--   sourceEid
--   params
--   artifactKind
--   attemptCount
--   maxAttempts
--   leaseOwner
--   leaseExpiresAt

type LeaseNextNodeRaw =
  ( Int64
  , Int64
  , Int64
  , Text
  , Text
  , Text
  , Int32
  , Maybe Text
  , Maybe UUID
  , Ae.Value
  , Text
  , Int32
  , Int32
  , Text
  , UTCTime
  )

--------------------------------------------------------------------------------
-- Render input shape
--
--   ord
--   inputKind       source | node
--   refKind         narration | dialogue | visual | render_node
--   refEid
--   refDeriveKey
--   role

type RenderInputRaw =
  ( Int32
  , Text
  , Text
  , Maybe UUID
  , Maybe Text
  , Maybe Text
  )

--------------------------------------------------------------------------------
-- Lease next node
--
-- Parameters:
--
--   1 owner
--   2 lane              generate | fuse | finalize
--   3 maybe exec filter ai_tts | ai_image | ffmpeg_segment | ffmpeg_concat | ...
--   4 lease seconds
--
-- This is the preferred new statement.
--
-- Workers that want to consume any work in a lane pass Nothing for exec.
-- Specialized workers pass Just "ffmpeg_segment", Just "ffmpeg_concat", etc.
--
-- Source-input existence is checked again at lease time so a node promoted
-- before a source was removed does not get executed after it became invalid.

leaseNextNodeStmt :: Statement (Text, Text, Maybe Text, Int32) (Maybe LeaseNextNodeRaw)
leaseNextNodeStmt =
  [TH.maybeStatement|
    with candidate as (
      select
        n.uid
      from prod.render_node n
      join prod.render_job rj
        on rj.uid = n.render_job_fk
      where n.status = 'ready'
        and rj.status in ('running', 'open')
        and n.attempt_count < n.max_attempts
        and n.lane = $2::text
        and ($3::text? is null or n.exec = $3::text?)
        and prod.sourceinputsstillexist(n.uid)
      order by
        n.ord asc,
        n.uid asc
      limit 1
      for update of n skip locked
    )
    update prod.render_node n
    set status = 'leased',
        lease_owner = $1::text,
        lease_expires_at = now() + make_interval(secs => $4::int4),
        attempt_count = n.attempt_count + 1,
        updated_at = now(),
        error_text = null
    from candidate c
    where n.uid = c.uid
    returning
      n.render_job_fk::int8,
      (
        select rj.narration_fk
        from prod.render_job rj
        where rj.uid = n.render_job_fk
        limit 1
      )::int8,
      n.uid::int8,
      n.derive_key::text,
      n.lane::text,
      n.exec::text,
      n.ord::int4,
      n.source_kind::text?,
      n.source_eid::uuid?,
      n.params::jsonb,
      n.artifact_kind::text,
      n.attempt_count::int4,
      n.max_attempts::int4,
      n.lease_owner::text,
      n.lease_expires_at::timestamptz
  |]

--------------------------------------------------------------------------------
-- Compatibility lease statement
--
-- Parameters match the older WorkerCaps-style call:
--
--   1 owner
--   2 lane
--   3 hasGpu       ignored by the simplified schema
--   4 vramMb       ignored by the simplified schema
--   5 leaseSeconds
--
-- Use this if your WorkerLease module still calls the old 5-argument shape.
-- Prefer moving callers to leaseNextNodeStmt above when convenient.

-- Keep old capability parameters present for statement compatibility.

leaseNextNodeCompatStmt
  :: Statement
      (Text, Text, Bool, Maybe Int32, Int32)
      (Maybe LeaseNextNodeRaw)
leaseNextNodeCompatStmt =
  [TH.maybeStatement|
    with candidate as (
      select
        n.uid
      from prod.render_node n
      join prod.render_job rj
        on rj.uid = n.render_job_fk
      where n.status = 'ready'
        and rj.status in ('running', 'open')
        and n.attempt_count < n.max_attempts
        and n.lane = $2::text

        and ($3::bool = true or $3::bool = false)
        and ($4::int4? is null or $4::int4? is not null)

        and prod.sourceinputsstillexist(n.uid)
      order by
        n.ord asc,
        n.uid asc
      limit 1
      for update of n skip locked
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
      n.render_job_fk::int8,
      (
        select rj.narration_fk
        from prod.render_job rj
        where rj.uid = n.render_job_fk
        limit 1
      )::int8,
      n.uid::int8,
      n.derive_key::text,
      n.lane::text,
      n.exec::text,
      n.ord::int4,
      n.source_kind::text?,
      n.source_eid::uuid?,
      n.params::jsonb,
      n.artifact_kind::text,
      n.attempt_count::int4,
      n.max_attempts::int4,
      n.lease_owner::text,
      n.lease_expires_at::timestamptz
  |]

--------------------------------------------------------------------------------
-- Heartbeat
--
-- Keeps a lease alive. First heartbeat moves leased -> running.

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

--------------------------------------------------------------------------------
-- Complete success
--
-- On success:
--
--   * node becomes done
--   * node stores output asset_fk / asset_eid
--   * reusable artifact cache is upserted by (narration_fk, derive_key)
--
-- Parameters:
--
--   1 node uid
--   2 lease owner
--   3 asset uid
--   4 asset eid
--   5 optional request eid
--   6 optional notes

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
        n.render_job_fk,
        n.derive_key,
        n.lane,
        n.exec,
        n.artifact_kind
    ),
    meta as (
      select
        rj.narration_fk,
        u.derive_key,
        u.lane,
        u.exec,
        u.artifact_kind
      from updated u
      join prod.render_job rj on rj.uid = u.render_job_fk
    ),
    upsert_artifact as (
      insert into prod.render_artifact
        ( narration_fk
        , derive_key
        , lane
        , exec
        , artifact_kind
        , status
        , asset_fk
        , asset_eid
        , request_eid
        , notes
        )
      select
        m.narration_fk,
        m.derive_key,
        m.lane,
        m.exec,
        m.artifact_kind,
        'done',
        $3::int8,
        $4::uuid,
        $5::uuid?,
        $6::text?
      from meta m
      on conflict (narration_fk, derive_key)
      do update
        set lane = excluded.lane,
            exec = excluded.exec,
            artifact_kind = excluded.artifact_kind,
            status = 'done',
            asset_fk = excluded.asset_fk,
            asset_eid = excluded.asset_eid,
            request_eid = excluded.request_eid,
            notes = excluded.notes,
            updated_at = now()
      returning 1
    )
    select exists(select 1 from updated)::bool
  |]

--------------------------------------------------------------------------------
-- Complete failure
--
-- On retryable failure:
--
--   * node returns to ready if attempts remain
--
-- On terminal failure:
--
--   * node becomes failed
--   * render_artifact records failed status for the node's derive_key
--
-- Parameters:
--
--   1 node uid
--   2 lease owner
--   3 retryable
--   4 error text
--   5 optional notes
--   6 optional request eid

completeNodeFailureStmt :: Statement (Int64, Text, Bool, Text, Maybe Text, Maybe UUID) (Maybe Bool)
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
        n.render_job_fk,
        n.derive_key,
        n.lane,
        n.exec,
        n.artifact_kind,
        n.status
    ),
    meta as (
      select
        rj.narration_fk,
        u.derive_key,
        u.lane,
        u.exec,
        u.artifact_kind,
        u.status
      from updated u
      join prod.render_job rj on rj.uid = u.render_job_fk
    ),
    upsert_terminal_artifact as (
      insert into prod.render_artifact
        ( narration_fk
        , derive_key
        , lane
        , exec
        , artifact_kind
        , status
        , asset_fk
        , asset_eid
        , request_eid
        , notes
        )
      select
        m.narration_fk,
        m.derive_key,
        m.lane,
        m.exec,
        m.artifact_kind,
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
      where m.status = 'failed'
      on conflict (narration_fk, derive_key)
      do update
        set lane = excluded.lane,
            exec = excluded.exec,
            artifact_kind = excluded.artifact_kind,
            status = 'failed',
            asset_fk = null,
            asset_eid = null,
            request_eid = excluded.request_eid,
            notes = excluded.notes,
            updated_at = now()
      returning 1
    )
    select exists(select 1 from updated)::bool
  |]

--------------------------------------------------------------------------------
-- Recycle expired leases for one render job
--
-- Attempts are counted on lease acquisition, not on expiration.

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
    where n.render_job_fk = $1::int8
      and n.status in ('leased', 'running')
      and n.lease_expires_at is not null
      and n.lease_expires_at < now()
  |]

--------------------------------------------------------------------------------
-- Recycle expired leases globally
--
-- Useful for standalone workers. If a producer/manager ticker owns recycling,
-- use recycleExpiredLeasesStmt instead.

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

--------------------------------------------------------------------------------
-- Input lookup for task runners
--
-- Workers should now resolve execution inputs through prod.render_input rather
-- than through old payload fields.

selectNodeInputsStmt :: Statement Int64 (Vector RenderInputRaw)
selectNodeInputsStmt =
  [TH.vectorStatement|
    select
      ri.ord::int4,
      ri.input_kind::text,
      ri.ref_kind::text,
      ri.ref_eid::uuid?,
      ri.ref_derive_key::text?,
      ri.role::text?
    from prod.render_input ri
    where ri.node_fk = $1::int8
    order by ri.ord asc, ri.uid asc
  |]

--------------------------------------------------------------------------------
-- Upstream asset lookup
--
-- Parameters:
--
--   1 render job uid
--   2 upstream derive key
--
-- Used by fuse/finalize task runners to resolve node inputs to assets.

selectUpstreamNodeAssetStmt :: Statement (Int64, Text) (Maybe (Int64, UUID, Text))
selectUpstreamNodeAssetStmt =
  [TH.maybeStatement|
    select
      n.asset_fk::int8,
      n.asset_eid::uuid,
      n.artifact_kind::text
    from prod.render_node n
    where n.render_job_fk = $1::int8
      and n.derive_key = $2::text
      and n.status = 'done'
      and n.asset_fk is not null
      and n.asset_eid is not null
    limit 1
  |]