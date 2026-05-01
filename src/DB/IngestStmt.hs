{-# LANGUAGE QuasiQuotes #-}

module DB.IngestStmt
  ( upsertNarrationStmt
  , selectNarrationUidStmt
  , selectNarrationEidStmt
  , selectNarrationByNameStmt
  , selectDialogueIdentityRowsStmt
  , selectVisualIdentityRowsStmt
  , deleteDialogueTreeStmt
  , insertDialogueStmt
  , insertDialogueSentenceStmt
  , insertDialogueVisualStmt
  ) where

import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.UUID (UUID)
import Data.Time.Clock (UTCTime)
import Data.Vector (Vector)

import Hasql.Statement (Statement)
import qualified Hasql.TH as TH

--------------------------------------------------------------------------------
-- Narration root
--
-- Expected table:
--
-- create table if not exists prod.narration (
--   uid bigint generated always as identity primary key,
--   eid uuid not null unique,
--   title text,
--   language text not null,
--   notes text,
--   created_at timestamptz not null default now(),
--   updated_at timestamptz not null default now()
-- );

upsertNarrationStmt :: Statement (UUID, Maybe Text, Text, Maybe Text) Int64
upsertNarrationStmt =
  [TH.singletonStatement|
    insert into prod.narration
      (eid, title, language, notes)
    values
      ($1::uuid, $2::text?, $3::text, $4::text?)
    on conflict (eid) do update
      set title = excluded.title,
          language = excluded.language,
          notes = excluded.notes,
          updated_at = now()
    returning uid::int8
  |]

selectNarrationUidStmt :: Statement UUID (Maybe Int64)
selectNarrationUidStmt =
  [TH.maybeStatement|
    select n.uid::int8
    from prod.narration n
    where n.eid = $1::uuid
    limit 1
  |]


selectNarrationEidStmt :: Statement UUID (Maybe (Int64, Text, Text, UTCTime))
selectNarrationEidStmt =
  [TH.maybeStatement|
    select
      n.uid::int8, n.nickname::text, n.title::text, n.created_at::timestamptz
    from prod.narration n
    where n.eid = $1::uuid
  |]


selectNarrationByNameStmt :: Statement Text (Maybe (Int64, UUID, Text, UTCTime))
selectNarrationByNameStmt =
  [TH.maybeStatement|
    select
      n.uid::int8, n.eid::uuid, n.title::text, n.created_at::timestamptz
    from prod.narration n
    where n.title = $1::text
  |]


--------------------------------------------------------------------------------
-- Existing identity lookup
--
-- These statements are meant to be called before deleteDialogueTreeStmt.
--
-- The ingest operation should compute fingerprints from the newly parsed
-- Dialogue and DialogueVisual values, then match those fingerprints against
-- these rows to preserve stable source UUIDs across re-ingest.
--
-- Fingerprint collisions are possible if identical content appears more than
-- once. Therefore these statements return all rows, not only one row per
-- fingerprint. The ingest operation should treat the result as a multimap.

selectDialogueIdentityRowsStmt :: Statement Int64 (Vector (Text, UUID, Int32))
selectDialogueIdentityRowsStmt =
  [TH.vectorStatement|
    select
      d.fingerprint::text,
      d.eid::uuid,
      d.ord::int4
    from prod.dialogue d
    where d.narration_fk = $1::int8
    order by d.ord asc, d.uid asc
  |]

selectVisualIdentityRowsStmt :: Statement Int64 (Vector (Text, UUID, Int32, Maybe Int32))
selectVisualIdentityRowsStmt =
  [TH.vectorStatement|
    select
      v.fingerprint::text,
      v.eid::uuid,
      v.ord::int4,
      v.sentence_ord::int4?
    from prod.dialogue_visual v
    join prod.dialogue d on d.uid = v.dialogue_fk
    where d.narration_fk = $1::int8
    order by d.ord asc, v.ord asc, v.uid asc
  |]

--------------------------------------------------------------------------------
-- Replace dialogue tree
--
-- Because prod.dialogue_sentence and prod.dialogue_visual should both use
-- on delete cascade from prod.dialogue, deleting dialogue rows is sufficient.
-- This statement avoids preserving row uid values. Stable source identity is
-- provided by eid, which the ingest logic chooses before insertion.

deleteDialogueTreeStmt :: Statement Int64 ()
deleteDialogueTreeStmt =
  [TH.resultlessStatement|
    delete from prod.dialogue
    where narration_fk = $1::int8
  |]

--------------------------------------------------------------------------------
-- Dialogue insertion
--
-- Parser-side Dialogue has:
--
--   emotions :: [Text]
--   sentences :: [Text]
--   visuals :: [DialogueVisual]
--
-- The ingest operation should render emotions into the DB emotion field, for
-- example:
--
--   emotion = T.intercalate ", " dialogue.emotions
--
-- The ingest operation should also compute:
--
--   fingerprint = fingerprintDialogue dialogue
--
-- and provide the preserved or fresh eid.

insertDialogueStmt :: Statement (UUID, Int64, Int32, Text, Text) Int64
insertDialogueStmt =
  [TH.singletonStatement|
    insert into prod.dialogue
      (eid, narration_fk, ord, emotion, fingerprint)
    values
      ($1::uuid, $2::int8, $3::int4, $4::text, $5::text)
    returning uid::int8
  |]

insertDialogueSentenceStmt :: Statement (Int64, Int32, Text) Int64
insertDialogueSentenceStmt =
  [TH.singletonStatement|
    insert into prod.dialogue_sentence
      (dialogue_fk, ord, body)
    values
      ($1::int8, $2::int4, $3::text)
    returning uid::int8
  |]

--------------------------------------------------------------------------------
-- Visual insertion
--
-- Parser-side DialogueVisual has:
--
--   sentenceOrd :: Maybe Int32
--   description :: Text
--
-- The ingest operation should compute:
--
--   fingerprint = fingerprintVisual visual
--
-- and provide the preserved or fresh eid.

insertDialogueVisualStmt :: Statement (UUID, Int64, Int32, Maybe Int32, Text, Text) Int64
insertDialogueVisualStmt =
  [TH.singletonStatement|
    insert into prod.dialogue_visual
      (eid, dialogue_fk, ord, sentence_ord, body, fingerprint)
    values
      ($1::uuid, $2::int8, $3::int4, $4::int4?, $5::text, $6::text)
    returning uid::int8
  |]