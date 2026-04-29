{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pitcher.Ingest
  ( IngestReport(..)
  , ingest
  , parseNarration
  , validateNarration
  , dialogueFingerprint
  , visualFingerprint
  ) where

import Control.Exception (throwIO)
import Control.Monad (foldM, forM_, replicateM, unless, void, when)
import Control.Monad.Except (throwError, MonadError (catchError))
import Control.Monad.Error.Class (MonadError)

import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import Data.Int (Int32, Int64)
import qualified Data.Map.Strict as Mp
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.UUID (UUID)
import qualified Data.UUID as Uu
import qualified Data.UUID.V4 as U4
import qualified Data.Vector as Vc

import Hasql.Pool (Pool)
import qualified Hasql.Pool as Pool
import Hasql.Session (Session)
import qualified Hasql.Transaction as HT
import qualified Hasql.Transaction.Sessions as HTS

import Text.Megaparsec
  ( eof
  , errorBundlePretty
  , runParser
  )

import Options.Cli
  ( IngestOpts(..)
  )

import Pitcher.Ingest.Parser
  ( narrationP
  )

import Pitcher.NarrationTypes
  ( Dialogue(..)
  , DialogueVisual(..)
  , Narration(..)
  )

import qualified DB.IngestStmt as Is


instance MonadError Text HT.Transaction where
  throwError = fail . T.unpack
  catchError = catchError

instance MonadFail HT.Transaction where
  fail = throwError . T.pack



--------------------------------------------------------------------------------
-- Reports

data IngestReport = IngestReport
  { dialoguesCount :: Int
  , sentencesCount :: Int
  , visualsCount :: Int
  }
  deriving (Eq, Show)

emptyReport :: IngestReport
emptyReport =
  IngestReport
    { dialoguesCount = 0
    , sentencesCount = 0
    , visualsCount = 0
    }

renderReport :: IngestReport -> Text
renderReport report =
  tshow report.dialoguesCount <> " dialogues, "
    <> tshow report.sentencesCount <> " sentences, "
    <> tshow report.visualsCount <> " visuals"

summarizeNarration :: Narration -> IngestReport
summarizeNarration narration =
  foldl accumDialogue emptyReport narration.dialogues
  where
    accumDialogue :: IngestReport -> Dialogue -> IngestReport
    accumDialogue acc dialogue =
      IngestReport
        { dialoguesCount = acc.dialoguesCount + 1
        , sentencesCount = acc.sentencesCount + length dialogue.sentences
        , visualsCount = acc.visualsCount + length dialogue.visuals
        }

--------------------------------------------------------------------------------
-- Top-level command

ingest :: IngestOpts -> Pool -> IO ()
ingest opts pool = do
  sourceText <- readUtf8TextFile opts.inputPath
  narrationEid <- U4.nextRandom

  narration <- either (throwIO . userError) pure $ parseNarration opts.inputPath sourceText

  case validateNarration narration of
    Left errs -> throwIO . userError $ "Invalid narration:\n" <> T.unpack (T.unlines errs)
    Right () -> pure ()

  let previewReport = summarizeNarration narration

  if opts.validateOnly then do
    TIO.putStrLn $
      "Validated narration from "
        <> T.pack opts.inputPath
        <> ": "
        <> renderReport previewReport
    putStrLn $ "narration: " <> show narration
  else do
    -- narrationEid <- resolveNarrationEid opts.slug

    freshDialogueEids <- replicateM (length narration.dialogues) U4.nextRandom
    freshVisualEids <- replicateM (visualCount narration) U4.nextRandom

    report <- runPoolSession pool $
        HTS.transaction HTS.Serializable HTS.Write $ persistNarrationTx opts narrationEid freshDialogueEids freshVisualEids narration

    TIO.putStrLn $ "Ingested narration '" <> T.pack (Uu.toString narrationEid) <> "' from "
        <> T.pack opts.inputPath <> ": " <> renderReport report


resolveNarrationEid :: Text -> IO UUID
resolveNarrationEid txt =
  case Uu.fromText txt of
    Nothing ->
      throwIO . userError $
        "Narration identity must be a UUID; got: " <> T.unpack txt
    Just eid ->
      pure eid

visualCount :: Narration -> Int
visualCount narration =
  sum [ length dialogue.visuals | dialogue <- narration.dialogues ]

--------------------------------------------------------------------------------
-- Validation

validateNarration :: Narration -> Either [Text] ()
validateNarration narration =
  let errs =
        concat
          [ validateDialogue dialogueOrd dialogue
          | (dialogueOrd, dialogue) <- withOrd32 narration.dialogues
          ]
  in
    if null errs
      then Right ()
      else Left errs

validateDialogue :: Int32 -> Dialogue -> [Text]
validateDialogue dialogueOrd dialogue =
  let sentenceCount = length dialogue.sentences
  in
    concat
      [ validateVisual dialogueOrd visualOrd sentenceCount visual
      | (visualOrd, visual) <- withOrd32 dialogue.visuals
      ]

validateVisual :: Int32 -> Int32 -> Int -> DialogueVisual -> [Text]
validateVisual dialogueOrd visualOrd sentenceCount visual =
  case visual.sentenceOrd of
    Nothing ->
      []
    Just ix
      | ix < 1 ->
          [ "Dialogue "
              <> tshow dialogueOrd
              <> ", visual "
              <> tshow visualOrd
              <> " references sentence "
              <> tshow ix
              <> ", but sentence references start at 1."
          ]
      | fromIntegral ix > sentenceCount ->
          [ "Dialogue "
              <> tshow dialogueOrd
              <> ", visual "
              <> tshow visualOrd
              <> " references sentence "
              <> tshow ix
              <> ", but the dialogue has only "
              <> tshow sentenceCount
              <> " sentence(s)."
          ]
      | otherwise ->
          []

--------------------------------------------------------------------------------
-- Persistence

persistNarrationTx
  :: IngestOpts
  -> UUID
  -> [UUID]
  -> [UUID]
  -> Narration
  -> HT.Transaction IngestReport
persistNarrationTx opts narrationEid freshDialogueEids freshVisualEids narration = do
  narrationUid <-
    HT.statement
      ( narrationEid
      , nonBlankMaybe opts.title
      , opts.language
      , opts.speaker
      )
      Is.upsertNarrationStmt

  dialogueRows <-
    HT.statement
      narrationUid
      Is.selectDialogueIdentityRowsStmt

  visualRows <-
    HT.statement
      narrationUid
      Is.selectVisualIdentityRowsStmt

  plan <-
    case buildInsertPlan narration dialogueRows visualRows freshDialogueEids freshVisualEids of
      Left err ->
        fail (T.unpack err)
      Right ok ->
        pure ok

  HT.statement narrationUid Is.deleteDialogueTreeStmt

  foldM
    (insertDialogueTx narrationUid)
    emptyReport
    plan.dialogues

--------------------------------------------------------------------------------
-- Insert plan

data InsertPlan = InsertPlan
  { dialogues :: [DialogueInsert]
  }
  deriving (Eq, Show)

data DialogueInsert = DialogueInsert
  { eid :: UUID
  , ord :: Int32
  , emotion :: Text
  , fingerprint :: Text
  , sentences :: [(Int32, Text)]
  , visuals :: [VisualInsert]
  }
  deriving (Eq, Show)

data VisualInsert = VisualInsert
  { eid :: UUID
  , ord :: Int32
  , sentenceOrd :: Maybe Int32
  , description :: Text
  , fingerprint :: Text
  }
  deriving (Eq, Show)

data IdentityState = IdentityState
  { dialogueByFingerprint :: Mp.Map Text [UUID]
  , visualByFingerprint :: Mp.Map Text [UUID]
  , freshDialogueEids :: [UUID]
  , freshVisualEids :: [UUID]
  }
  deriving (Eq, Show)

buildInsertPlan
  :: Narration
  -> Vc.Vector (Text, UUID, Int32)
  -> Vc.Vector (Text, UUID, Int32, Maybe Int32)
  -> [UUID]
  -> [UUID]
  -> Either Text InsertPlan
buildInsertPlan narration dialogueRows visualRows freshDialogueEids freshVisualEids = do
  let initialState =
        IdentityState
          { dialogueByFingerprint =
              mkIdentityMap
                [ (fp, eid)
                | (fp, eid, _oldOrd) <- Vc.toList dialogueRows
                ]
          , visualByFingerprint =
              mkIdentityMap
                [ (fp, eid)
                | (fp, eid, _oldOrd, _oldSentenceOrd) <- Vc.toList visualRows
                ]
          , freshDialogueEids = freshDialogueEids
          , freshVisualEids = freshVisualEids
          }

  (finalState, dialogueInserts) <-
    foldM
      buildDialogueInsert
      (initialState, [])
      (withOrd32 narration.dialogues)

  pure InsertPlan
    { dialogues = dialogueInserts
    }

mkIdentityMap :: [(Text, UUID)] -> Mp.Map Text [UUID]
mkIdentityMap pairs =
  Mp.fromListWith
    (<>)
    [ (fp, [eid])
    | (fp, eid) <- pairs
    ]

buildDialogueInsert
  :: (IdentityState, [DialogueInsert])
  -> (Int32, Dialogue)
  -> Either Text (IdentityState, [DialogueInsert])
buildDialogueInsert (st0, acc) (dialogueOrd, dialogue) = do
  let fp = dialogueFingerprint dialogue

  (dialogueEid, st1) <- takeDialogueEid fp st0

  (st2, visualInserts) <-
    foldM
      buildVisualInsert
      (st1, [])
      (withOrd32 dialogue.visuals)

  let dialogueInsert =
        DialogueInsert
          { eid = dialogueEid
          , ord = dialogueOrd
          , emotion = renderEmotions dialogue.emotions
          , fingerprint = fp
          , sentences =
              [ (sentenceOrd, sentence)
              | (sentenceOrd, sentence) <- withOrd32 dialogue.sentences
              ]
          , visuals = visualInserts
          }

  pure (st2, acc <> [dialogueInsert])

buildVisualInsert
  :: (IdentityState, [VisualInsert])
  -> (Int32, DialogueVisual)
  -> Either Text (IdentityState, [VisualInsert])
buildVisualInsert (st0, acc) (visualOrd, visual) = do
  let fp = visualFingerprint visual

  (visualEid, st1) <- takeVisualEid fp st0

  let visualInsert =
        VisualInsert
          { eid = visualEid
          , ord = visualOrd
          , sentenceOrd = visual.sentenceOrd
          , description = visual.description
          , fingerprint = fp
          }

  pure (st1, acc <> [visualInsert])

takeDialogueEid :: Text -> IdentityState -> Either Text (UUID, IdentityState)
takeDialogueEid fp st =
  case takeExisting fp st.dialogueByFingerprint of
    Just (eid, remaining) ->
      Right
        ( eid
        , st { dialogueByFingerprint = remaining }
        )
    Nothing ->
      case st.freshDialogueEids of
        [] ->
          Left "Internal error: not enough fresh dialogue UUIDs were generated."
        eid : rest ->
          Right
            ( eid
            , st { freshDialogueEids = rest }
            )

takeVisualEid :: Text -> IdentityState -> Either Text (UUID, IdentityState)
takeVisualEid fp st =
  case takeExisting fp st.visualByFingerprint of
    Just (eid, remaining) ->
      Right
        ( eid
        , st { visualByFingerprint = remaining }
        )
    Nothing ->
      case st.freshVisualEids of
        [] ->
          Left "Internal error: not enough fresh visual UUIDs were generated."
        eid : rest ->
          Right
            ( eid
            , st { freshVisualEids = rest }
            )

takeExisting :: Text -> Mp.Map Text [UUID] -> Maybe (UUID, Mp.Map Text [UUID])
takeExisting fp mp =
  case Mp.lookup fp mp of
    Nothing ->
      Nothing
    Just [] ->
      Nothing
    Just (eid : rest) ->
      let mp' =
            if null rest
              then Mp.delete fp mp
              else Mp.insert fp rest mp
      in
        Just (eid, mp')

insertDialogueTx
  :: Int64
  -> IngestReport
  -> DialogueInsert
  -> HT.Transaction IngestReport
insertDialogueTx narrationUid report dialogue = do
  dialogueUid <-
    HT.statement
      ( dialogue.eid
      , narrationUid
      , dialogue.ord
      , dialogue.emotion
      , dialogue.fingerprint
      )
      Is.insertDialogueStmt

  forM_ dialogue.sentences $ \(sentenceOrd, body) ->
    void $
      HT.statement
        (dialogueUid, sentenceOrd, body)
        Is.insertDialogueSentenceStmt

  forM_ dialogue.visuals $ \visual ->
    void $
      HT.statement
        ( visual.eid
        , dialogueUid
        , visual.ord
        , visual.sentenceOrd
        , visual.description
        , visual.fingerprint
        )
        Is.insertDialogueVisualStmt

  pure
    IngestReport
      { dialoguesCount = report.dialoguesCount + 1
      , sentencesCount = report.sentencesCount + length dialogue.sentences
      , visualsCount = report.visualsCount + length dialogue.visuals
      }

--------------------------------------------------------------------------------
-- Fingerprints
--
-- These deliberately ignore uid and ord so content identity survives row
-- replacement and reordering.

dialogueFingerprint :: Dialogue -> Text
dialogueFingerprint dialogue =
  canonicalFingerprint
    [ "dialogue"
    , T.intercalate "\x1e" (map normalizeForFingerprint dialogue.emotions)
    , T.intercalate "\x1e" (map normalizeForFingerprint dialogue.sentences)
    ]

visualFingerprint :: DialogueVisual -> Text
visualFingerprint visual =
  canonicalFingerprint
    [ "visual"
    , maybe "" tshow visual.sentenceOrd
    , normalizeForFingerprint visual.description
    ]

canonicalFingerprint :: [Text] -> Text
canonicalFingerprint =
  T.intercalate "\x1f" . map escapeFingerprintPart

escapeFingerprintPart :: Text -> Text
escapeFingerprintPart =
  T.concatMap escapeChar
  where
    escapeChar '\x1f' = "\\x1f"
    escapeChar '\x1e' = "\\x1e"
    escapeChar '\\' = "\\\\"
    escapeChar c = T.singleton c

normalizeForFingerprint :: Text -> Text
normalizeForFingerprint =
  T.toLower . T.unwords . T.words

renderEmotions :: [Text] -> Text
renderEmotions emotions =
  T.intercalate ", " (map (T.unwords . T.words) emotions)

--------------------------------------------------------------------------------
-- File loading / parsing

readUtf8TextFile :: FilePath -> IO Text
readUtf8TextFile path = do
  bytes <- BS.readFile path
  case TE.decodeUtf8' bytes of
    Left err ->
      throwIO . userError $
        "Could not decode input file as UTF-8: " <> show err
    Right txt ->
      pure (dropBom txt)

dropBom :: Text -> Text
dropBom txt =
  fromMaybe txt $
    T.stripPrefix "\xfeff" txt

parseNarration :: FilePath -> Text -> Either String Narration
parseNarration inputPath sourceText =
  first errorBundlePretty $
    runParser (narrationP <* eof) inputPath sourceText

--------------------------------------------------------------------------------
-- DB session helper

runPoolSession :: Pool -> Session a -> IO a
runPoolSession pool session = do
  result <- Pool.use pool session
  case result of
    Left err ->
      throwIO . userError $
        "Database session failed: " <> show err
    Right value ->
      pure value

--------------------------------------------------------------------------------
-- Small helpers

withOrd32 :: [a] -> [(Int32, a)]
withOrd32 =
  zip [1..]

nonBlankMaybe :: Text -> Maybe Text
nonBlankMaybe txt =
  let txt' = T.strip txt
  in
    if T.null txt'
      then Nothing
      else Just txt'

tshow :: Show a => a -> Text
tshow =
  T.pack . show