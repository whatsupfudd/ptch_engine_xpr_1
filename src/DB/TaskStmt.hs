{-# LANGUAGE QuasiQuotes #-}
module DB.TaskStmt where

import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.UUID (UUID)
import Data.Vector (Vector)

import qualified Data.Aeson as Ae

import Hasql.Statement (Statement)
import qualified Hasql.TH as TH


selectInputAssetStmt :: Statement (Int64, Text) (Maybe (Int64, UUID))
selectInputAssetStmt =
  [TH.maybeStatement|
    select n.asset_fk::int8, n.asset_eid::uuid
    from prod.render_node n
    where n.graph_fk = $1::int8
      and n.key = $2::text
      and n.status = 'done'
      and n.asset_fk is not null
      and n.asset_eid is not null
    limit 1
  |]

selectDialogueSentenceBodiesStmt :: Statement Int64 (Vector (Int32, Text))
selectDialogueSentenceBodiesStmt =
  [TH.vectorStatement|
    select s.ord::int4, s.body::text
    from prod.dialogue_sentence s
    where s.dialogue_fk = $1::int8
    order by s.ord asc
  |]

selectDialogueVisualAnchorsStmt :: Statement Int64 (Vector (Int32, Maybe Int32))
selectDialogueVisualAnchorsStmt =
  [TH.vectorStatement|
    select v.ord::int4, v.sentence_ord::int4?
    from prod.dialogue_visual v
    where v.dialogue_fk = $1::int8
    order by v.ord asc
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


selectDialogueSentenceBodiesByEidStmt :: Statement UUID (Vector (Int32, Text))
selectDialogueSentenceBodiesByEidStmt =
  [TH.vectorStatement|
    select
      s.ord::int4,
      s.body::text
    from prod.dialogue_sentence s
    join prod.dialogue d on d.uid = s.dialogue_fk
    where d.eid = $1::uuid
    order by s.ord asc
  |]

selectDialogueVisualAnchorsByDialogueEidStmt :: Statement UUID (Vector (Int32, Maybe Int32))
selectDialogueVisualAnchorsByDialogueEidStmt =
  [TH.vectorStatement|
    select
      v.ord::int4,
      v.sentence_ord::int4?
    from prod.dialogue_visual v
    join prod.dialogue d on d.uid = v.dialogue_fk
    where d.eid = $1::uuid
    order by v.ord asc
  |]

selectVisualDescriptionByEidStmt :: Statement UUID (Maybe Text)
selectVisualDescriptionByEidStmt =
  [TH.maybeStatement|
    select v.body::text
    from prod.dialogue_visual v
    where v.eid = $1::uuid
    limit 1
  |]


selectVizContextsByVisualEid :: Statement UUID (Vector (Text, Int32, Text))
selectVizContextsByVisualEid =
  [TH.vectorStatement|
    select
      vc.kind::text, vc.seqnum::int4, vc.content::text
    from prod.dialogue_visual dv
    join prod.dialogue d on d.uid = dv.dialogue_fk
    join prod.vizcontext vc on vc.narration_fk = d.narration_fk
    where dv.eid = $1::uuid
    order by vc.kind desc, vc.seqnum asc
  |]


selectVisualOwnerAndAnchorByEidStmt :: Statement UUID (UUID, Maybe Int32)
selectVisualOwnerAndAnchorByEidStmt =
  [TH.singletonStatement|
    select
      d.eid::uuid
      , v.sentence_ord::int4?
    from prod.dialogue_visual v
    join prod.dialogue d on d.uid = v.dialogue_fk
    where v.eid = $1::uuid
    limit 1
  |]
