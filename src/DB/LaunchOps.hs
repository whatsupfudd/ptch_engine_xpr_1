module DB.LaunchOps where

import Control.Monad (void, when, replicateM)

import Data.Int (Int64, Int32)
import qualified Data.List as L
import Data.Ord (comparing)
import qualified Data.Map.Strict as Mp
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as Uu
import qualified Data.Vector as Vc
import qualified Data.UUID.V4 as U4

import qualified Data.Aeson as Ae

import Hasql.Pool (Pool)
import Hasql.Session (Session, statement)

import DB.Helpers (runSessionOrThrow)
import Pitcher.NarrationTypes (DialogueRender (..), VisualRender (..), NarrationRender (..))
import Pitcher.Render.TaskTypes (PersistedRenderState)
import DB.LaunchStmt


loadNarrationRender :: Pool -> (Int64, UUID) -> IO NarrationRender
loadNarrationRender pool (narrationUid, nEid) = do
  dialogueRows <- runSessionOrThrow "selectDialoguesStmt" pool $ statement narrationUid selectDialoguesStmt
  sentenceRows <- runSessionOrThrow "selectSentencesStmt" pool $ statement narrationUid selectSentencesStmt
  visualRows <- runSessionOrThrow "selectVisualsStmt" pool $ statement narrationUid selectVisualsStmt

  let
    sentenceMap = Vc.foldl' (\acc (dialogueFk, ord, body) ->
          Mp.insertWith (<>) dialogueFk [(ord, body)] acc
        ) Mp.empty sentenceRows

    visualMap = Vc.foldl' (\acc (uid, dialogueFk, eid, sentenceIx, body) ->
          let
            nextOrd = 1 + maybe 0 length (Mp.lookup dialogueFk acc)
            vis = VisualRender { uid = uid, ord = fromIntegral nextOrd, eid = eid, sentenceIx = sentenceIx, description = body }
          in
          Mp.insertWith (<>) dialogueFk [vis] acc
        ) Mp.empty visualRows

    dialogues = [ DialogueRender { uid = uid, eid = eid, ord = ord, emotion = emotion
            , sentences = map snd . L.sortOn fst . fromMaybe [] $ Mp.lookup uid sentenceMap
            , visuals = fromMaybe [] $ Mp.lookup uid visualMap
            }
          | (uid, eid, ord, emotion) <- Vc.toList dialogueRows
          ]

  pure NarrationRender { narrationUid = narrationUid, eid = nEid,dialogues = L.sortBy (comparing (.ord)) dialogues }


loadOrCreateRenderJob :: Pool -> Int64 -> IO Int64
loadOrCreateRenderJob pool narrationUid = do
  mbFound <- runSessionOrThrow "findRenderJobStmt" pool $ statement narrationUid findRenderJobStmt
  case mbFound of
    Just uid -> pure uid
    Nothing -> runSessionOrThrow "createRenderJobStmt" pool $ statement (narrationUid, "running" :: Text, Ae.object [] :: Ae.Value) createRenderJobStmt


persistRenderJobState
  :: Pool
  -> Int64
  -> Text
  -> PersistedRenderState
  -> Maybe Int64
  -> IO ()
persistRenderJobState pool jobUid status st mbFinalUid = do
  void . runSessionOrThrow "updateRenderJobStateStmt" pool $ statement (jobUid, status, Ae.toJSON st, mbFinalUid) updateRenderJobStateStmt


lookupReusableArtifact
  :: Pool
  -> Int64
  -> Text
  -> Maybe Int64
  -> Maybe Int32
  -> Text
  -> IO (Maybe (Int64, UUID))
lookupReusableArtifact pool jobUid kind mbDialogue mbVisualOrd sourceSig =
  runSessionOrThrow "lookupReusableArtifactStmt" pool $ statement (jobUid, kind, mbDialogue, mbVisualOrd, sourceSig) lookupReusableArtifactStmt

writeArtifactRecord
  :: Pool
  -> Int64
  -> Text
  -> Maybe Int64
  -> Maybe Int32
  -> Text
  -> Text
  -> Maybe Int64
  -> Maybe UUID
  -> Maybe UUID
  -> Maybe Text
  -> IO ()
writeArtifactRecord pool jobUid kind mbDialogue mbVisualOrd sourceSig status mbAssetUid mbAssetEid mbReqEid mbNotes = do
  when (jobUid /= 0) $
    void $ runSessionOrThrow "deleteArtifactStmt" pool $ statement (jobUid, kind, mbDialogue, mbVisualOrd) deleteArtifactStmt
  when (jobUid /= 0) $
    void $ runSessionOrThrow "insertArtifactStmt" pool $ statement
        (jobUid, kind, mbDialogue, mbVisualOrd, sourceSig, status, mbAssetUid, mbAssetEid, mbReqEid, mbNotes)
        insertArtifactStmt


insertAssetRow :: Pool -> Text -> UUID -> Text -> Int64 -> Text -> IO Int64
insertAssetRow pool name eid contentType size notes =
  runSessionOrThrow "insertAssetStmt" pool $ statement (Just name, eid, Nothing :: Maybe Text, contentType, size, Just notes) insertAssetStmt


lookupAssetUidByEidIO :: Pool -> UUID -> IO (Maybe Int64)
lookupAssetUidByEidIO pool eid =
  runSessionOrThrow "lookupAssetUidStmt" pool $ statement eid lookupAssetUidStmt
