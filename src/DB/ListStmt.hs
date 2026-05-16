{-# LANGUAGE QuasiQuotes #-}
module DB.ListStmt where

import Data.Int (Int32,Int64)
import Data.Text (Text)
import Data.UUID (UUID)
import Data.Time.Clock (UTCTime)
import qualified Data.Vector as Vc

import Hasql.Statement (Statement)
import qualified Hasql.TH as TH

selectNarrationsStmt :: Statement () (Vc.Vector (Int64, UUID, Text, Text, UTCTime))
selectNarrationsStmt =
  [TH.vectorStatement|
    select
      uid::int8, eid::uuid, nickname::text, title::text, created_at::timestamptz
    from prod.narration
    order by created_at desc
  |]

fetchDialoguesStmt :: Statement Int64 (Vc.Vector (Int64, UUID, Int32, Text))
fetchDialoguesStmt =
  [TH.vectorStatement|
    select
      uid::int8, eid::uuid, ord::int4, emotion::text
    from prod.dialogue
    where narration_fk = $1::int8
    order by ord asc
  |]

type RenderNodeRaw = (Int64, Maybe UUID, Text, Text, Text, UTCTime, Int32, Int32, Maybe Text)
fetchRenderNodesStmt :: Statement Int64 (Vc.Vector RenderNodeRaw)
fetchRenderNodesStmt =
  [TH.vectorStatement|
    select
      n.uid::int8, n.source_eid::uuid?, n.exec::text, n.lane::text
      , n.status::text, n.created_at::timestamptz, n.max_attempts::int4, n.attempt_count::int4
      , n.error_text::text?
    from prod.render_node n
    join prod.render_job j on j.uid = n.render_job_fk
    where j.narration_fk = $1::int8
      and j.status = 'running'
    order by created_at desc
  |]


fetchRenderNodesByJobStmt :: Statement Int64 (Vc.Vector RenderNodeRaw)
fetchRenderNodesByJobStmt =
  [TH.vectorStatement|
    select
      n.uid::int8, n.source_eid::uuid?, n.exec::text, n.lane::text
      , n.status::text, n.created_at::timestamptz, n.max_attempts::int4, n.attempt_count::int4
      , n.error_text::text?
    from prod.render_node n
    where n.render_job_fk = $1::int8
    order by created_at desc
  |]
