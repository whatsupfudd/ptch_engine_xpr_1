{-# LANGUAGE QuasiQuotes #-}

module DB.RefinalizeStmt where

import qualified Data.Aeson as Ae
import Data.Int (Int64)
import Data.Text (Text)
import Data.UUID (UUID)
import Data.Vector (Vector)

import Hasql.Statement (Statement)
import qualified Hasql.TH as TH


type FinalNodeRefinalizeRaw =
  ( Int64
  , Int64
  , UUID
  , Text
  )


selectFinalNodeRefinalizeStmt
  :: Statement UUID (Maybe FinalNodeRefinalizeRaw)
selectFinalNodeRefinalizeStmt =
  [TH.maybeStatement|
    select
      rj.uid::int8,
      rn.uid::int8,
      n.eid::uuid,
      rn.derive_key::text
    from prod.render_job rj
    join prod.narration n
      on n.uid = rj.narration_fk
    join prod.render_node rn
      on rn.render_job_fk = rj.uid
    where rj.eid = $1::uuid
      and rj.status = 'completed'
      and rn.lane = 'finalize'
      and rn.exec = 'ffmpeg_concat'
      and rn.artifact_kind = 'final'
      and rn.status = 'done'
      and (
        select count(*)
        from prod.render_node rn2
        where rn2.render_job_fk = rj.uid
          and rn2.lane = 'finalize'
          and rn2.artifact_kind = 'final'
      ) = 1
      and not exists (
        select 1
        from prod.render_job newer
        where newer.narration_fk = rj.narration_fk
          and (
            newer.created_at > rj.created_at
            or (
              newer.created_at = rj.created_at
              and newer.uid > rj.uid
            )
          )
      )
    limit 1
    for update of rj, rn
  |]


selectSegmentKeysFinalStmt :: Statement Int64 (Vector Text)
selectSegmentKeysFinalStmt =
  [TH.vectorStatement|
    select ri.ref_derive_key::text
    from prod.render_input ri
    where ri.node_fk = $1::int8
      and ri.input_kind = 'node'
      and ri.ref_kind = 'render_node'
      and ri.role = 'segment'
      and ri.ref_derive_key is not null
    order by ri.ord asc
  |]


reopenFinalizationStmt
  :: Statement
      ( UUID
      , Int64
      , Text
      , Text
      , Ae.Value
      )
      (Maybe (Int64, Int64, Int64))
reopenFinalizationStmt =
  [TH.maybeStatement|
    with target as (
      select
        rj.uid as job_uid,
        rj.narration_fk,
        rn.uid as node_uid
      from prod.render_job rj
      join prod.render_node rn
        on rn.render_job_fk = rj.uid
      where rj.eid = $1::uuid
        and rj.status = 'completed'
        and rn.uid = $2::int8
        and rn.status = 'done'
        and rn.lane = 'finalize'
        and rn.artifact_kind = 'final'
        and md5(rn.derive_key) = md5($3::text)
      for update of rj, rn
    ),
    cache_invalidated as (
      delete from prod.render_artifact ra
      using target t
      where ra.narration_fk = t.narration_fk
        and (
          md5(ra.derive_key) = md5($3::text)
          or md5(ra.derive_key) = md5($4::text)
        )
      returning ra.uid
    ),
    node_updated as (
      update prod.render_node rn
      set
        derive_key = $4::text,
        params = $5::jsonb,
        status = 'ready',
        attempt_count = 0,
        lease_owner = null,
        lease_expires_at = null,
        asset_fk = null,
        asset_eid = null,
        completed_at = null,
        error_text = null,
        updated_at = now()
      from target t
      where rn.uid = t.node_uid
      returning rn.uid
    ),
    job_updated as (
      update prod.render_job rj
      set
        status = 'running',
        final_asset_fk = null,
        completed_at = null,
        updated_at = now()
      from target t, node_updated nu
      where rj.uid = t.job_uid
      returning rj.uid
    )
    select
      ju.uid::int8,
      nu.uid::int8,
      (select count(*) from cache_invalidated)::int8
    from job_updated ju
    cross join node_updated nu
  |]