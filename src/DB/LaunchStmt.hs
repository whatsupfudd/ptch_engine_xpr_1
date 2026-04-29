{-# LANGUAGE QuasiQuotes #-}
module DB.LaunchStmt where

import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as Uu
import qualified Data.Vector as Vc

import qualified Data.Aeson as Ae

import Hasql.Session (Session, statement)
import Hasql.Statement (Statement)
import qualified Hasql.TH as TH


--------------------------------------------------------------------------------
-- SQL statements
--
-- Assumed Pitcher additions:
--
-- create table if not exists prod.render_job (
--   uid bigint generated always as identity primary key,
--   narration_fk bigint not null references prod.narration(uid),
--   status text not null,
--   state jsonb not null default '{}'::jsonb,
--   final_asset_fk int null references asset(uid),
--   created_at timestamptz not null default now(),
--   updated_at timestamptz not null default now()
-- );
--
-- create table if not exists prod.render_artifact (
--   uid bigint generated always as identity primary key,
--   render_job_fk bigint not null references prod.render_job(uid),
--   kind text not null,
--   dialogue_fk bigint null references prod.dialogue(uid),
--   visual_ord int null,
--   source_sig text not null,
--   status text not null,
--   asset_fk int null references asset(uid),
--   asset_eid uuid null,
--   request_eid uuid null,
--   notes text null,
--   created_at timestamptz not null default now(),
--   updated_at timestamptz not null default now()
-- );

-- From Narration EID:
selectNarrationUidStmt :: Statement UUID (Maybe Int64)
selectNarrationUidStmt =
  [TH.maybeStatement|
    select uid::int8
    from prod.narration
    where eid = $1::uuid
    order by uid desc
    limit 1
  |]


-- From NarrationID:
selectDialoguesStmt :: Statement Int64 (Vc.Vector (Int64, UUID, Int32, Text))
selectDialoguesStmt =
  [TH.vectorStatement|
    select d.uid::int8, d.eid::uuid, d.ord::int4, d.emotion::text
    from prod.dialogue d
    where d.narration_fk = $1::int8
    order by d.ord asc
  |]

-- From NarrationID:
selectSentencesStmt :: Statement Int64 (Vc.Vector (Int64, Int32, Text))
selectSentencesStmt =
  [TH.vectorStatement|
    select s.dialogue_fk::int8, s.ord::int4, s.body::text
    from prod.dialogue_sentence s
    join prod.dialogue d on d.uid = s.dialogue_fk
    where d.narration_fk = $1::int8
    order by s.dialogue_fk asc, s.ord asc
  |]


selectVisualsStmt :: Statement Int64 (Vc.Vector (Int64, Int64, UUID, Maybe Int32, Text))
selectVisualsStmt =
  [TH.vectorStatement|
    select
      v.uid::int8, v.dialogue_fk::int8, v.eid::uuid, v.sentence_ord::int4?, v.body::text
    from prod.dialogue_visual v
    join prod.dialogue d on d.uid = v.dialogue_fk
    where d.narration_fk = $1::int8
    order by v.dialogue_fk asc, v.uid asc
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


updateRenderJobStateStmt :: Statement (Int64, Text, Ae.Value, Maybe Int64) ()
updateRenderJobStateStmt =
  [TH.resultlessStatement|
    update prod.render_job
    set status = $2::text,
        state = $3::jsonb,
        final_asset_fk = $4::int8?,
        updated_at = now()
    where uid = $1::int8
  |]


lookupReusableArtifactStmt :: Statement (Int64, Text, Maybe Int64, Maybe Int32, Text) (Maybe (Int64, UUID))
lookupReusableArtifactStmt =
  [TH.maybeStatement|
    select asset_fk::int8, asset_eid::uuid
    from prod.render_artifact
    where render_job_fk = $1::int8
      and kind = $2::text
      and dialogue_fk is not distinct from $3::int8?
      and visual_ord is not distinct from $4::int4?
      and source_sig = $5::text
      and status = 'done'
      and asset_fk is not null
      and asset_eid is not null
    order by updated_at desc
    limit 1
  |]


deleteArtifactStmt :: Statement (Int64, Text, Maybe Int64, Maybe Int32) ()
deleteArtifactStmt =
  [TH.resultlessStatement|
    delete from prod.render_artifact
    where render_job_fk = $1::int8
      and kind = $2::text
      and dialogue_fk is not distinct from $3::int8?
      and visual_ord is not distinct from $4::int4?
  |]


insertArtifactStmt
  :: Statement
      ( Int64
      , Text
      , Maybe Int64
      , Maybe Int32
      , Text
      , Text
      , Maybe Int64
      , Maybe UUID
      , Maybe UUID
      , Maybe Text
      )
      ()
insertArtifactStmt =
  [TH.resultlessStatement|
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
    values
      ( $1::int8
      , $2::text
      , $3::int8?
      , $4::int4?
      , $5::text
      , $6::text
      , $7::int8?
      , $8::uuid?
      , $9::uuid?
      , $10::text?
      )
  |]


insertAssetStmt :: Statement (Maybe Text, UUID, Maybe Text, Text, Int64, Maybe Text) Int64
insertAssetStmt =
  [TH.singletonStatement|
    insert into asset
      (name, eid, description, contentType, size, notes)
    values
      ($1::text?, $2::uuid, $3::text?, $4::text, $5::int8, $6::text?)
    returning uid::int8
  |]


lookupAssetUidStmt :: Statement UUID (Maybe Int64)
lookupAssetUidStmt =
  [TH.maybeStatement|
    select uid::int8
    from asset
    where eid = $1::uuid
    order by version desc
    limit 1
  |]
