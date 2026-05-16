module Commands.List (listCmd) where

import qualified Control.Monad.Cont as Mc

import Data.Int (Int64)
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as Uu
import Data.Time.Clock (UTCTime)
import qualified Data.Vector as Vc

import Hasql.Pool (Pool, use)
import qualified Hasql.Session as Hs

import Options.Cli (FilterSubCmd (..), ListOpts (..), NarrationIdOpt (..))
import Options.Runtime (RunOptions (..))
import DB.Connect (startPg)

import qualified DB.ListStmt as Ls
import qualified DB.IngestStmt as Is
import Data.Yaml (ParseException(_anchorName))


data Narration = Narration
  { uid :: Int64
  , eid :: UUID
  , nickname :: Text
  , title :: Text
  , createdAt :: UTCTime
  }
  deriving (Eq, Show)


listCmd :: ListOpts -> RunOptions -> IO ()
listCmd opts rtOpts =
  let
    pgPool = startPg rtOpts.pgDbConf
  in
  Mc.runContT pgPool (listNarrations opts rtOpts)


listNarrations :: ListOpts -> RunOptions -> Pool -> IO ()
listNarrations opts rtOpts dbPool = do
  narrationSet <- case opts.target of
    Nothing -> do
      -- get all narrations
      eiRez <- use dbPool $ Hs.statement () Ls.selectNarrationsStmt
      case eiRez of
        Left err -> error $ "@[listNarrations] selectNarrationsStmt err: " <> show err
        Right vRows -> pure $ Vc.map
            (\(uid, eid, nickname, title, createdAt) -> Narration uid eid nickname title createdAt)
            vRows

    Just target ->
      -- get narration by target
      case target of
        EidNI tEid -> do
          case Uu.fromText tEid of
            Nothing -> error $ "@[listNarrations] invalid eid: " <> show tEid
            Just eid -> do
              eiRez <- use dbPool $ Hs.statement eid Is.selectNarrationEidStmt
              case eiRez of
                Left err -> error $ "@[listNarrations] selectNarrationByEidStmt err: " <> show err
                Right mbRows ->
                  case mbRows of
                    Nothing -> error $ "@[listNarrations] narration not found: " <> show tEid
                    Just (uid, nickname, title, createdAt) ->
                      pure . Vc.singleton $ Narration uid eid nickname title createdAt
        NameNI name -> do
          eiRez <- use dbPool $ Hs.statement name Is.selectNarrationByNameStmt
          case eiRez of
            Left err -> error $ "@[listNarrations] selectNarrationByNameStmt err: " <> show err
            Right mbRows ->
              case mbRows of
                Nothing -> error $ "@[listNarrations] narration not found: " <> show name
                Just (uid, eid, title, createdAt) -> pure . Vc.singleton $ Narration uid eid name title createdAt

  listWithNarrations dbPool narrationSet opts.filter rtOpts


listWithNarrations :: Pool -> Vc.Vector Narration -> Maybe FilterSubCmd -> RunOptions -> IO ()
listWithNarrations dbPool narrationSet mbFilter rtOpts = do
  case mbFilter of
    Nothing -> mapM_ print narrationSet
    Just filter ->
      case filter of
        DialogueFC -> mapM_ (listDialogues dbPool . uid) narrationSet
        RenderNodeFC lane status Nothing -> mapM_ (listRenderNodes dbPool lane status . uid) narrationSet
        RenderNodeFC lane status (Just jobUid) -> listRenderNodesForJob dbPool lane status jobUid


listDialogues :: Pool -> Int64 -> IO ()
listDialogues dbPool narrationUid = do
  rezA <- use dbPool $ Hs.statement narrationUid Ls.fetchDialoguesStmt
  case rezA of
    Left err -> error $ "@[listDialogues] fetchDialoguesStmt err: " <> show err
    Right vRows -> mapM_ print vRows
  

listRenderNodes :: Pool -> Maybe Text -> Maybe Text -> Int64 -> IO ()
listRenderNodes dbPool mbLane mbStatus narrationUid = do
  rezA <- use dbPool $ Hs.statement narrationUid Ls.fetchRenderNodesStmt

  case rezA of
    Left err -> error $ "@[listRenderNodes] fetchRenderNodesStmt err: " <> show err
    Right vRows ->
      let
        filteredRows = case (mbLane, mbStatus) of
          (Just lane, Just status) -> Vc.filter (\(_, _, _, lane', status', _, _, _, _) -> lane' == lane && status' == status) vRows
          (Just lane, Nothing) -> Vc.filter (\(_, _, _, lane', _, _, _, _, _) -> lane' == lane) vRows
          (Nothing, Just status) -> Vc.filter (\(_, _, _, _, status', _, _, _, _) -> status' == status) vRows
          (Nothing, Nothing) -> vRows
      in
      mapM_ print filteredRows

listRenderNodesForJob :: Pool -> Maybe Text -> Maybe Text -> Int64 -> IO ()
listRenderNodesForJob dbPool mbLane mbStatus jobUid = do
  rezA <- use dbPool $ Hs.statement jobUid Ls.fetchRenderNodesByJobStmt
  case rezA of
    Left err -> error $ "@[listRenderNodesForJob] fetchRenderNodesByJobStmt err: " <> show err
    Right vRows ->
      let
        filteredRows = case (mbLane, mbStatus) of
          (Just lane, Just status) -> Vc.filter (\(_, _, _, lane', status', _, _, _, _) -> lane' == lane && status' == status) vRows
          (Just lane, Nothing) -> Vc.filter (\(_, _, _, lane', _, _, _, _, _) -> lane' == lane) vRows
          (Nothing, Just status) -> Vc.filter (\(_, _, _, _, status', _, _, _, _) -> status' == status) vRows
          (Nothing, Nothing) -> vRows
      in
      mapM_ print filteredRows

showRenderNode :: Ls.RenderNodeRaw -> String
showRenderNode (uid, sourceEid, exec, lane, status, createdAt, maxAttempts, attemptCount, errorText) =
  "UID: " <> show uid <> " "
  <> "Source EID: " <> show sourceEid <> " "
  <> "Exec: " <> show exec <> " "
  <> "Lane: " <> show lane <> " "
  <> "Status: " <> show status <> " "
  <> "Created At: " <> show createdAt <> " "
  <> "Max Attempts: " <> show maxAttempts <> " "
  <> "Attempt Count: " <> show attemptCount <> " "
  <> "Error Text: " <> show errorText