{-# LANGUAGE QuasiQuotes #-}

module DB.ProducerStmt where

import qualified Data.Aeson as Ae
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.UUID (UUID)
import Data.Vector (Vector)

import Hasql.Statement (Statement)
import qualified Hasql.TH as TH

--------------------------------------------------------------------------------
-- Narration lookup

selectNarrationUidStmt :: Statement UUID (Maybe Int64)
selectNarrationUidStmt =
  [TH.maybeStatement|
    select n.uid::int8
    from prod.narration n
    where n.eid = $1::uuid
    limit 1
  |]

selectVizContextsStmt :: Statement Int64 (Vector (Text, Int32, Text))
selectVizContextsStmt =
  [TH.vectorStatement|
    select
      vc.kind::text, vc.seqnum::int4, vc.content::text
    from prod.vizcontext vc
    where vc.narration_fk = $1::int8
    order by vc.kind asc, vc.seqnum asc
  |]

--------------------------------------------------------------------------------
-- Render job creation
--
-- New design:
--   * create one fresh render_job per producer launch
--   * older running jobs for the same narration can be marked superseded
--   * reusable artifacts are independent of job identity

createRenderJobStmt :: Statement Int64 Int64
createRenderJobStmt =
  [TH.singletonStatement|
    insert into prod.render_job
      (narration_fk, status, supersedes_job_fk)
    values
      ( $1::int8
      , 'running'
      , (
          select rj.uid
          from prod.render_job rj
          where rj.narration_fk = $1::int8
          order by rj.created_at desc, rj.uid desc
          limit 1
        )
      )
    returning uid::int8
  |]

markPreviousJobsSupersededStmt :: Statement Int64 Int64
markPreviousJobsSupersededStmt =
  [TH.rowsAffectedStatement|
    update prod.render_job old
    set status = 'superseded',
        updated_at = now()
    where old.narration_fk = (
        select cur.narration_fk
        from prod.render_job cur
        where cur.uid = $1::int8
      )
      and old.uid <> $1::int8
      and old.status in ('running', 'open')
  |]

selectOpenRenderJobStmt :: Statement Int64 (Maybe Int64)
selectOpenRenderJobStmt =
  [TH.maybeStatement|
    select rj.uid::int8
    from prod.render_job rj
    where rj.narration_fk = $1::int8
      and rj.status in ('running', 'open')
    order by rj.created_at desc, rj.uid desc
    limit 1
  |]

--------------------------------------------------------------------------------
-- Locking

tryAdvisoryJobLockStmt :: Statement Int32 Bool
tryAdvisoryJobLockStmt =
  [TH.singletonStatement|
    select pg_try_advisory_xact_lock(91741, $1::int4)::bool
  |]

--------------------------------------------------------------------------------
-- Node insertion
--
-- Parameters:
--
--   1  render_job_fk
--   2  derive_key
--   3  lane               generate | fuse | finalize
--   4  exec
--   5  ord
--   6  source_kind        narration | dialogue | visual | null
--   7  source_eid         uuid | null
--   8  params             jsonb
--   9  artifact_kind      audio | image | segment | final | ...
--   10 max_attempts
--
-- Because each render launch creates a fresh render_job, this insert does not need
-- to reset previously-done nodes. The old upsert/reset complexity disappears.

insertRenderNodeStmt
  :: Statement
      ( Int64
      , Text
      , Text
      , Text
      , Int32
      , Maybe Text
      , Maybe UUID
      , Ae.Value
      , Text
      , Int32
      )
      Int64
insertRenderNodeStmt =
  [TH.singletonStatement|
    insert into prod.render_node
      ( render_job_fk
      , derive_key
      , lane
      , exec
      , ord
      , source_kind
      , source_eid
      , params
      , artifact_kind
      , status
      , max_attempts
      )
    values
      ( $1::int8
      , $2::text
      , $3::text
      , $4::text
      , $5::int4
      , $6::text?
      , $7::uuid?
      , $8::jsonb
      , $9::text
      , 'pending'
      , $10::int4
      )
    returning uid::int8
  |]

--------------------------------------------------------------------------------
-- Input insertion
--
-- Revised render_input shape:
--
--   source input:
--     input_kind = 'source'
--     ref_kind = 'narration' | 'dialogue' | 'visual'
--     ref_eid is not null
--     ref_derive_key is null
--
--   node input:
--     input_kind = 'node'
--     ref_kind = 'render_node'
--     ref_eid is null
--     ref_derive_key is not null
--
-- Parameters:
--
--   1 node_fk
--   2 ord
--   3 input_kind
--   4 ref_kind
--   5 ref_eid
--   6 ref_derive_key
--   7 role

insertRenderInputStmt :: Statement (Int64, Int32, Text, Text, Maybe UUID, Maybe Text, Maybe Text) ()
insertRenderInputStmt =
  [TH.resultlessStatement|
    insert into prod.render_input
      ( node_fk
      , ord
      , input_kind
      , ref_kind
      , ref_eid
      , ref_derive_key
      , role
      )
    values
      ( $1::int8
      , $2::int4
      , $3::text
      , $4::text
      , $5::uuid?
      , $6::text?
      , $7::text?
      )
  |]

--------------------------------------------------------------------------------
-- Reuse existing artifacts
--
-- This is the main simplification.
--
-- A node is reusable when:
--   * same narration
--   * same derive_key
--   * artifact exists and is done
--
-- No dialogue_fk, visual_ord, uid, ord, or duplicated source_sig matching.

markReusableNodesDoneStmt :: Statement Int64 Int64
markReusableNodesDoneStmt =
  [TH.rowsAffectedStatement|
    with job as (
      select rj.uid, rj.narration_fk
      from prod.render_job rj
      where rj.uid = $1::int8
    ),
    reusable as (
      select distinct on (ra.derive_key)
        ra.derive_key,
        ra.asset_fk,
        ra.asset_eid
      from prod.render_artifact ra
      join job j on j.narration_fk = ra.narration_fk
      where ra.status = 'done'
        and ra.asset_fk is not null
        and ra.asset_eid is not null
      order by
        ra.derive_key,
        ra.updated_at desc,
        ra.uid desc
    )
    update prod.render_node n
    set status = 'done',
        asset_fk = r.asset_fk,
        asset_eid = r.asset_eid,
        completed_at = now(),
        updated_at = now(),
        error_text = null
    from reusable r
    where n.render_job_fk = $1::int8
      and n.status in ('pending', 'ready')
      and n.derive_key = r.derive_key
  |]

--------------------------------------------------------------------------------
-- Promote ready nodes
--
-- Source inputs do not block readiness.
-- Node inputs block readiness until the referenced upstream node in the same job
-- is done or skipped.

promoteReadyNodesStmt :: Statement Int64 Int64
promoteReadyNodesStmt =
  [TH.rowsAffectedStatement|
    with promotable as (
      select n.uid
      from prod.render_node n
      where n.render_job_fk = $1::int8
        and n.status = 'pending'
        and not exists (
          select 1
          from prod.render_input ri
          left join prod.render_node upstream
            on upstream.render_job_fk = n.render_job_fk
           and upstream.derive_key = ri.ref_derive_key
          where ri.node_fk = n.uid
            and ri.input_kind = 'node'
            and (
              upstream.uid is null
              or upstream.status not in ('done', 'skipped')
            )
        )
        and prod.sourceInputsStillExist(n.uid)
    )
    update prod.render_node n
    set status = 'ready',
        updated_at = now()
    where n.uid in (select uid from promotable)
  |]

--------------------------------------------------------------------------------
-- Recycle expired leases
--
-- Attempts are counted when a worker leases the node, not when the lease expires.
-- Expiry only makes the node ready again, or terminally failed if attempts are
-- already exhausted.

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
-- Finalize render job
--
-- A job is completed only when every node is done/skipped.
-- A job is failed if any node is failed.
-- Otherwise it remains running.
--
-- The final asset is taken from the done finalize node.

finalizeRenderJobStmt :: Statement Int64 Bool
finalizeRenderJobStmt =
  [TH.singletonStatement|
    with stats as (
      select
        coalesce(bool_and(n.status in ('done', 'skipped')), false) as all_done,
        coalesce(bool_or(n.status = 'failed'), false) as any_failed,
        max(n.asset_fk) filter (
          where n.lane = 'finalize'
            and n.artifact_kind = 'final'
            and n.status = 'done'
        ) as final_asset_fk
      from prod.render_node n
      where n.render_job_fk = $1::int8
    ),
    updated_job as (
      update prod.render_job rj
      set status =
            case
              when s.all_done then 'completed'
              when s.any_failed then 'failed'
              else 'running'
            end,
          final_asset_fk =
            case
              when s.all_done then s.final_asset_fk
              else null
            end,
          completed_at =
            case
              when s.all_done then now()
              else null
            end,
          updated_at = now()
      from stats s
      where rj.uid = $1::int8
      returning s.all_done::bool
    )
    select coalesce((select all_done from updated_job), false)::bool
  |]

selectFinalAssetStmt :: Statement Int64 (Maybe (Int64, UUID))
selectFinalAssetStmt =
  [TH.maybeStatement|
    select a.uid::int8, a.eid::uuid
    from prod.render_job rj
    join asset a on a.uid = rj.final_asset_fk
    where rj.uid = $1::int8
      and rj.status = 'completed'
    limit 1
  |]