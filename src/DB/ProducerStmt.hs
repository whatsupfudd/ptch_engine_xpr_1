{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
module DB.ProducerStmt where

import Data.Int (Int64, Int32)
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.Vector as Vc

import qualified Data.Aeson as Ae
import Data.Aeson ((.=))

import Hasql.Session (Session, statement)
import Hasql.Statement (Statement)
import qualified Hasql.TH as TH
import qualified Hasql.Transaction as HT
import qualified Hasql.Transaction.Sessions as HTS

--------------------------------------------------------------------------------
-- SQL
--
-- Assumed graph tables:
--
-- create table prod.render_graph (
--   uid bigint generated always as identity primary key,
--   render_job_fk bigint not null references prod.render_job(uid) on delete cascade,
--   schema_ver int not null,
--   status text not null default 'open',
--   created_at timestamptz not null default now(),
--   updated_at timestamptz not null default now(),
--   unique (render_job_fk)
-- );
--
-- create table prod.render_node (
--   uid bigint generated always as identity primary key,
--   graph_fk bigint not null references prod.render_graph(uid) on delete cascade,
--   key text not null,
--   stage text not null,
--   exec text not null,
--   ord int not null,
--   dialogue_fk bigint null references prod.dialogue(uid) on delete cascade,
--   visual_ord int null,
--   artifact_kind text null,
--   source_sig text not null,
--   requirements jsonb not null,
--   payload jsonb not null,
--   status text not null default 'pending',
--   max_attempts int not null,
--   attempt_count int not null default 0,
--   lease_owner text null,
--   lease_expires_at timestamptz null,
--   asset_fk bigint null references asset(uid) on delete set null,
--   asset_eid uuid null,
--   completed_at timestamptz null,
--   error_text text null,
--   created_at timestamptz not null default now(),
--   updated_at timestamptz not null default now(),
--   unique (graph_fk, key)
-- );
--
-- create table prod.render_edge (
--   uid bigint generated always as identity primary key,
--   graph_fk bigint not null references prod.render_graph(uid) on delete cascade,
--   from_node_fk bigint not null references prod.render_node(uid) on delete cascade,
--   to_node_fk bigint not null references prod.render_node(uid) on delete cascade,
--   unique (graph_fk, from_node_fk, to_node_fk)
-- );


selectNarrationUidStmt :: Statement UUID (Maybe Int64)
selectNarrationUidStmt =
  [TH.maybeStatement|
    select uid::int8
    from prod.narration
    where eid = $1::uuid
    order by uid desc
    limit 1
  |]


selectDialoguesStmt :: Statement Int64 (Vc.Vector (Int64, Int32, Text))
selectDialoguesStmt =
  [TH.vectorStatement|
    select d.uid::int8, d.ord::int4, d.emotion::text
    from prod.dialogue d
    where d.narration_fk = $1::int8
    order by d.ord asc
  |]

selectSentencesStmt :: Statement Int64 (Vc.Vector (Int64, Int32, Text))
selectSentencesStmt =
  [TH.vectorStatement|
    select s.dialogue_fk::int8, s.ord::int4, s.body::text
    from prod.dialogue_sentence s
    join prod.dialogue d on d.uid = s.dialogue_fk
    where d.narration_fk = $1::int8
    order by s.dialogue_fk asc, s.ord asc
  |]

selectVisualsStmt :: Statement Int64 (Vc.Vector (Int64, Int32, Maybe Int32, Text))
selectVisualsStmt =
  [TH.vectorStatement|
    select v.dialogue_fk::int8, v.ord::int4, v.sentence_ord::int4?, v.body::text
    from prod.dialogue_visual v
    join prod.dialogue d on d.uid = v.dialogue_fk
    where d.narration_fk = $1::int8
    order by v.dialogue_fk asc, v.ord asc
  |]

findRenderJobStmt :: Statement Int64 (Maybe Int64)
findRenderJobStmt =
  [TH.maybeStatement|
    select uid::int8
    from prod.render_job
    where narration_fk = $1::int8
    order by uid desc
    limit 1
  |]

createRenderJobStmt :: Statement (Int64, Text, Ae.Value) Int64
createRenderJobStmt =
  [TH.singletonStatement|
    insert into prod.render_job
      (narration_fk, status, state)
    values
      ($1::int8, $2::text, $3::jsonb)
    returning uid::int8
  |]

tryAdvisoryJobLockStmt :: Statement Int64 Bool
tryAdvisoryJobLockStmt =
  [TH.singletonStatement|
    select pg_try_advisory_xact_lock($1::int8)::bool
  |]

insertRenderGraphStmt :: Statement (Int64, Int32) Int64
insertRenderGraphStmt =
  [TH.singletonStatement|
    insert into prod.render_graph
      (render_job_fk, schema_ver, status)
    values
      ($1::int8, $2::int4, 'open')
    on conflict (render_job_fk) do update
      set schema_ver = excluded.schema_ver,
          updated_at = now()
    returning uid::int8
  |]

findGraphUidStmt :: Statement Int64 (Maybe Int64)
findGraphUidStmt =
  [TH.maybeStatement|
    select uid::int8
    from prod.render_graph
    where render_job_fk = $1::int8
    limit 1
  |]

insertRenderNodeStmt
  :: Statement
      ( Int64
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
      )
      Int64
insertRenderNodeStmt =
  [TH.singletonStatement|
    insert into prod.render_node
      ( graph_fk
      , key
      , stage
      , exec
      , ord
      , dialogue_fk
      , visual_ord
      , artifact_kind
      , source_sig
      , requirements
      , payload
      , max_attempts
      )
    values
      ( $1::int8
      , $2::text
      , $3::text
      , $4::text
      , $5::int4
      , $6::int8?
      , $7::int4?
      , $8::text?
      , $9::text
      , $10::jsonb
      , $11::jsonb
      , $12::int4
      )
    on conflict (graph_fk, key) do update
      set stage = excluded.stage,
          exec = excluded.exec,
          ord = excluded.ord,
          dialogue_fk = excluded.dialogue_fk,
          visual_ord = excluded.visual_ord,
          artifact_kind = excluded.artifact_kind,
          source_sig = excluded.source_sig,
          requirements = excluded.requirements,
          payload = excluded.payload,
          max_attempts = excluded.max_attempts,
          updated_at = now()
    returning uid::int8
  |]

deleteRenderEdgesStmt :: Statement Int64 ()
deleteRenderEdgesStmt =
  [TH.resultlessStatement|
    delete from prod.render_edge
    where graph_fk = $1::int8
  |]

insertRenderEdgeStmt :: Statement (Int64, Int64, Int64) ()
insertRenderEdgeStmt =
  [TH.resultlessStatement|
    insert into prod.render_edge
      (graph_fk, from_node_fk, to_node_fk)
    values
      ($1::int8, $2::int8, $3::int8)
    on conflict (graph_fk, from_node_fk, to_node_fk) do nothing
  |]

recycleExpiredLeasesStmt :: Statement Int64 Int64
recycleExpiredLeasesStmt =
  [TH.rowsAffectedStatement|
    update prod.render_node n
      set status = case
            when n.attempt_count + 1 >= n.max_attempts then 'failed'
            else 'ready'
          end
        , attempt_count = n.attempt_count + 1
        , lease_owner = null
        , lease_expires_at = null
        , error_text = case
            when n.attempt_count + 1 >= n.max_attempts
              then coalesce(n.error_text, '') || '\nLease expired too many times.'
            else n.error_text
          end
        , updated_at = now()
    where n.graph_fk = $1::int8
      and n.status in ('leased', 'running')
      and n.lease_expires_at is not null
      and n.lease_expires_at < now()
  |]

markReusableNodesDoneStmt :: Statement Int64 Int64
markReusableNodesDoneStmt =
  [TH.rowsAffectedStatement|
    with g as (
      select rg.uid, rj.narration_fk
      from prod.render_graph rg
      join prod.render_job rj on rj.uid = rg.render_job_fk
      where rg.uid = $1::int8
    ),
    reusable as (
      select distinct on
        (ra.kind, ra.dialogue_fk, ra.visual_ord, ra.source_sig)
        ra.kind,
        ra.dialogue_fk,
        ra.visual_ord,
        ra.source_sig,
        ra.asset_fk,
        ra.asset_eid
      from prod.render_artifact ra
      join prod.render_job rj2 on rj2.uid = ra.render_job_fk
      join g on g.narration_fk = rj2.narration_fk
      where ra.status = 'done'
        and ra.asset_fk is not null
        and ra.asset_eid is not null
      order by
        ra.kind,
        ra.dialogue_fk,
        ra.visual_ord,
        ra.source_sig,
        ra.updated_at desc
    )
    update prod.render_node n
    set status = 'done',
        asset_fk = r.asset_fk,
        asset_eid = r.asset_eid,
        completed_at = now(),
        updated_at = now()
    from reusable r
    where n.graph_fk = $1::int8
      and n.status in ('pending', 'ready')
      and n.artifact_kind is not distinct from r.kind
      and n.dialogue_fk is not distinct from r.dialogue_fk
      and n.visual_ord is not distinct from r.visual_ord
      and n.source_sig = r.source_sig
  |]

promoteReadyNodesStmt :: Statement Int64 Int64
promoteReadyNodesStmt =
  [TH.rowsAffectedStatement|
    with promotable as (
      select n.uid
      from prod.render_node n
      where n.graph_fk = $1::int8
        and n.status = 'pending'
        and not exists (
          select 1
          from prod.render_edge e
          join prod.render_node src on src.uid = e.from_node_fk
          where e.to_node_fk = n.uid
            and src.status not in ('done', 'skipped')
        )
    )
    update prod.render_node n
    set status = 'ready',
        updated_at = now()
    where n.uid in (select uid from promotable)
  |]

finalizeGraphIfDoneStmt :: Statement Int64 Bool
finalizeGraphIfDoneStmt =
  [TH.singletonStatement|
    with stats as (
      select
        bool_and(n.status in ('done', 'skipped')) as all_done,
        bool_or(n.status = 'failed') as any_failed,
        max(n.asset_fk) filter (where n.artifact_kind = 'final' and n.status = 'done') as final_asset_fk
      from prod.render_graph rg
      join prod.render_node n on n.graph_fk = rg.uid
      where rg.render_job_fk = $1::int8
    ),
    upd_graph as (
      update prod.render_graph rg
      set status =
            case
              when s.all_done then 'done'
              when s.any_failed then 'failed'
              else 'open'
            end,
          updated_at = now()
      from stats s
      where rg.render_job_fk = $1::int8
      returning s.all_done, s.any_failed, s.final_asset_fk
    )
    update prod.render_job rj
    set status =
          case
            when ug.all_done then 'completed'
            when ug.any_failed then 'failed'
            else rj.status
          end,
        final_asset_fk =
          case
            when ug.all_done then ug.final_asset_fk
            else rj.final_asset_fk
          end,
        updated_at = now()
    from upd_graph ug
    where rj.uid = $1::int8
    returning coalesce(ug.all_done, false)::bool
  |]
  