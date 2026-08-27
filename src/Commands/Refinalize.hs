module Commands.Refinalize (refinalizeCmd) where

import qualified Control.Monad.Cont as Mc
import Control.Exception (throwIO)

import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as Uu
import qualified Data.Vector as Vc

import Hasql.Pool (Pool)
import qualified Hasql.Transaction as HT

import DB.Connect (startPg)
import DB.Helpers (runTx)
import qualified DB.RefinalizeStmt as Rs

import Options.Cli (NarrationIdOpt (..))
import Options.Runtime (RunOptions (..))

import Pitcher.Render.Producer (ProducerCfg, defaultProducerCfg, deriveKeyFinal, paramsFinal)


data RefinalizeResult = RefinalizeResult
  { uidJob :: Int64
  , uidNode :: Int64
  , deriveKeyOld :: Text
  , deriveKeyNew :: Text
  , cacheInvalidated :: Int64
  }
  deriving (Eq, Show)


refinalizeCmd :: NarrationIdOpt -> RunOptions -> IO ()
refinalizeCmd opts rtOpts =
  case opts of
    EidNI txtNarrationEid ->
      case Uu.fromText txtNarrationEid of
        Nothing -> throwIO . userError $ "@[refinalizeCmd] invalid narration EID: " <> T.unpack txtNarrationEid
        Just narrationEid ->
          let
            pgPool = startPg rtOpts.pgDbConf
          in
          Mc.runContT pgPool $ refinalizeProduction defaultProducerCfg narrationEid
    NameNI txtNarrationName ->
      throwIO . userError $ "@[refinalizeCmd] find-by-title: unimplemented (" <> T.unpack txtNarrationName <> ")"


refinalizeProduction :: ProducerCfg -> Uu.UUID -> Pool -> IO ()
refinalizeProduction cfg prodEid pool = do
  result <-
    runTx "refinalizeProduction" pool $
      refinalizeTx cfg prodEid

  case result of
    Left err ->
      throwIO . userError $
        "@[refinalizeProduction] " <> T.unpack err

    Right rez -> do
      putStrLn "Production reopened for finalization."
      putStrLn $
        "  production EID: " <> Uu.toString prodEid
      putStrLn $
        "  render job UID: " <> show rez.uidJob
      putStrLn $
        "  finalize node UID: " <> show rez.uidNode
      putStrLn $
        "  cache entries invalidated: "
          <> show rez.cacheInvalidated

      if rez.deriveKeyOld == rez.deriveKeyNew
        then
          putStrLn "  final derive key: unchanged"
        else do
          putStrLn "  final derive key: updated"
          putStrLn $
            "    old: " <> T.unpack rez.deriveKeyOld
          putStrLn $
            "    new: " <> T.unpack rez.deriveKeyNew

      putStrLn ""
      putStrLn $
        "Finalize node is ready for "
          <> "`narravid work --lane finalize`."


refinalizeTx
  :: ProducerCfg
  -> Uu.UUID
  -> HT.Transaction (Either Text RefinalizeResult)
refinalizeTx cfg prodEid = do
  mbFinal <-
    HT.statement prodEid Rs.selectFinalNodeRefinalizeStmt

  case mbFinal of
    Nothing ->
      pure . Left $
        "production is not the latest completed production, "
          <> "or it does not contain exactly one completed final node."

    Just (uidJob, uidNode, eidNarration, keyOld) -> do
      segmentKeysV <-
        HT.statement uidNode Rs.selectSegmentKeysFinalStmt

      let
        segmentKeys = Vc.toList segmentKeysV

      if null segmentKeys
        then
          pure . Left $
            "finalize node contains no segment inputs."

        else do
          let
            keyNew =
              deriveKeyFinal cfg eidNarration segmentKeys

            paramsNew =
              paramsFinal cfg

          mbReopened <-
            HT.statement
              ( prodEid
              , uidNode
              , keyOld
              , keyNew
              , paramsNew
              )
              Rs.reopenFinalizationStmt

          case mbReopened of
            Nothing ->
              pure . Left $
                "production changed while the refinalize "
                  <> "operation was being prepared."

            Just (uidJob', uidNode', cacheCount) ->
              pure . Right $
                RefinalizeResult
                  { uidJob = uidJob'
                  , uidNode = uidNode'
                  , deriveKeyOld = keyOld
                  , deriveKeyNew = keyNew
                  , cacheInvalidated = cacheCount
                  }