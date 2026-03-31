{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}

module Pitcher.Ingest ( Narration(..), DialogueBlock(..), DialogueVisual(..), ingest) where

import Control.Exception (throwIO)
import Control.Monad (foldM, forM_, unless, void, when)
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import Data.Int (Int64, Int32)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding  as TE
import qualified Data.Text.IO as TIO
import Data.Void (Void)

import Hasql.Pool (Pool)
import qualified Hasql.Pool as Pool
import Hasql.Session (Session)
import Hasql.Statement (Statement)
import qualified Hasql.TH as TH
import qualified Hasql.Transaction as HT
import qualified Hasql.Transaction.Sessions as HTS

import Text.Megaparsec ( Parsec, eof, errorBundlePretty, lookAhead, manyTill, runParser, takeWhile1P
    , takeWhileP, try, (<|>), some, many )
import Text.Megaparsec.Char ( char, eol, hspace, hspace1, string, space )

import Options.Cli (IngestOpts (..))
import Data.Char (isDigit)


newtype Narration = Narration {
    dialogues :: [DialogueBlock]
  }
  deriving (Eq, Show)

data DialogueBlock = DialogueBlock {
    emotion :: Text
  , sentences :: [Text]
  , visuals :: [DialogueVisual]
  }
  deriving (Eq, Show)


data DialogueVisual = DialogueVisual {
    sentenceOrd :: Maybe Int32
  , description :: Text
  }
  deriving (Eq, Show)


data RawItem =
    RawContent Text
  | RawVisual DialogueVisual
  deriving (Eq, Show)


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

--------------------------------------------------------------------------------
-- Top-level command

ingest :: IngestOpts -> Pool -> IO ()
ingest opts pool = do
  sourceText <- readUtf8TextFile opts.inputPath
  narration <- either (throwIO . userError) pure $ parseNarration opts.inputPath sourceText
  let
    previewReport = summarizeNarration narration

  if opts.validateOnly then do
      TIO.putStrLn $ "Validated narration from " <> T.pack opts.inputPath
        <> ": " <> renderReport previewReport
      putStrLn $ "narration: " <> show narration
  else do
    report <- runPoolSession pool $ HTS.transaction HTS.Serializable HTS.Write $ persistNarrationTx opts narration
    TIO.putStrLn $ "Ingested narration '" <> opts.slug <> "' from " <> T.pack opts.inputPath
            <> ": " <> renderReport report


renderReport :: IngestReport -> Text
renderReport report = tshow report.dialoguesCount <> " dialogues, "
    <> tshow report.sentencesCount <> " sentences, "
    <> tshow report.visualsCount <> " visuals"


runPoolSession :: Pool -> Session a -> IO a
runPoolSession pool session = do
  result <- Pool.use pool session
  case result of
    Left err -> throwIO . userError $ "Database session failed: " <> show err
    Right value -> pure value


persistNarrationTx :: IngestOpts -> Narration -> HT.Transaction IngestReport
persistNarrationTx opts narration = do
  narrationUid <- HT.statement (opts.slug, opts.title, opts.language, opts.speaker) upsertKeynoteStmt
  HT.statement narrationUid deleteDialogueTreeStmt
  foldM (insertDialogueTx narrationUid) emptyReport $ zip [1 ..] narration.dialogues


insertDialogueTx :: Int64 -> IngestReport -> (Int32, DialogueBlock) -> HT.Transaction IngestReport
insertDialogueTx narrationUid report (dialogueOrd, dialogue) = do
  dialogueUid <- HT.statement (narrationUid, dialogueOrd, dialogue.emotion) insertDialogueStmt
  forM_ (zip [1 ..] dialogue.sentences) $ \(sentenceOrd, body) ->
    HT.statement (dialogueUid, sentenceOrd, body) insertDialogueSentenceStmt
  forM_ (zip [1 ..] dialogue.visuals) $ \(visualOrd, visual) -> HT.statement (dialogueUid, visualOrd, fromMaybe 1 visual.sentenceOrd, visual.description) insertDialogueVisualStmt
  pure $ IngestReport {
        dialoguesCount = report.dialoguesCount + 1
      , sentencesCount = report.sentencesCount + length dialogue.sentences
      , visualsCount = report.visualsCount + length dialogue.visuals
      }


summarizeNarration :: Narration -> IngestReport
summarizeNarration narration =
  foldl accumDialogue emptyReport narration.dialogues
  where
  accumDialogue :: IngestReport -> DialogueBlock -> IngestReport
  accumDialogue acc dialogue = IngestReport {
      dialoguesCount = acc.dialoguesCount + 1
    , sentencesCount = acc.sentencesCount + length dialogue.sentences
    , visualsCount = acc.visualsCount + length dialogue.visuals
    }


readUtf8TextFile :: FilePath -> IO Text
readUtf8TextFile path = do
  bytes <- BS.readFile path
  case TE.decodeUtf8' bytes of
    Left err -> throwIO . userError $
        "Could not decode input file as UTF-8: " <> show err
    Right txt -> pure (dropBom txt)


dropBom :: Text -> Text
dropBom txt = fromMaybe txt $ T.stripPrefix "\xfeff" txt


type NarrationParser = Parsec Void Text

parseNarration :: FilePath -> Text -> Either String Narration
parseNarration inputPath sourceText =
  first errorBundlePretty $ runParser narrationP inputPath sourceText


narrationP :: NarrationParser Narration
narrationP = do
  skipBlankLines
  blocks <- some dialogueBlockP
  skipBlankLines
  eof
  pure $ Narration blocks


dialogueBlockP :: NarrationParser DialogueBlock
dialogueBlockP = do
  dialogueHeaderP
  skipBlankLines
  rawItems <- manyTill rawLineP endOfDialogueP
  skipBlankLines
  either fail pure $ rawItemsToDialogue (catMaybes rawItems)


rawLineP :: NarrationParser (Maybe RawItem)
rawLineP =
      Nothing <$ try blankLineP
  <|> Just <$> try visualIndexedLineP
  <|> Just <$> try visualLineP
  <|> Just . RawContent <$> contentLineP


dialogueHeaderP :: NarrationParser ()
dialogueHeaderP = do
  hspace
  void $ string "[dialogue]"
  hspace
  lineEndP


visualLineP :: NarrationParser RawItem
visualLineP = do
  hspace
  void $ string "[visuals:"
  hspace
  desc <- restOfVisualLineP
  -- void $ char ']'
  let
    desc' = normalizeInline desc
  when (T.null desc') $
    fail "Empty [visuals:] description."
  pure $ RawVisual DialogueVisual {
      sentenceOrd = Nothing
    , description = desc'
    }


visualIndexedLineP :: NarrationParser RawItem
visualIndexedLineP = do
  hspace
  void $ string "[visuals("
  digits <- takeWhile1P (Just "visual sentence index") isAsciiDigit
  void $ string "):"
  hspace
  desc <- restOfVisualLineP
  -- void $ char ']'
  let
    desc' = normalizeInline desc
  when (T.null desc') $
    fail "Empty indexed visual description."
  pure $ RawVisual DialogueVisual {
      sentenceOrd = Just (read (T.unpack digits))
    , description = desc'
    }


contentLineP :: NarrationParser Text
contentLineP = do
  txt <- restOfLineP
  let
    stripped = T.strip txt
  when (T.isPrefixOf "[visuals" stripped) $ fail "Malformed visuals annotation."
  when (stripped == "[dialogue]") $ fail "Unexpected [dialogue] marker inside dialogue body."
  pure stripped


endOfDialogueP :: NarrationParser ()
endOfDialogueP = lookAhead $ skipBlankLines *> (void dialogueHeaderP <|> eof)


blankLineP :: NarrationParser ()
blankLineP = do
  hspace
  void eol


skipBlankLines :: NarrationParser ()
skipBlankLines = void $ many blankLineP


lineEndP :: NarrationParser ()
lineEndP = void eol <|> eof


restOfLineP :: NarrationParser Text
restOfLineP =
  takeWhileP (Just "line content") (\c -> c /= '\n' && c /= '\r') <* lineEndP

restOfVisualLineP :: NarrationParser Text
restOfVisualLineP =
  takeWhileP (Just "visual line content") (\c -> c /= '\n' && c /= '\r') <* lineEndP


rawItemsToDialogue :: [RawItem] -> Either String DialogueBlock
rawItemsToDialogue items = do
  let
    contentLines = [ line | RawContent line <- items, not (T.null (T.strip line)) ]
    visuals = [ visual | RawVisual visual <- items ]

  firstLine <- case contentLines of
      [] -> Left "@[rawItemsToDialogue] Dialogue block has no spoken content."
      line : _ -> Right line

  (emotionText, firstSentence) <- parseEmotionLine firstLine

  let
    otherSentences = concatMap splitContentLine (drop 1 contentLines)
    sentenceList = filter (not . T.null) $ map normalizeInline (firstSentence : otherSentences)

  when (null sentenceList) $ Left "@[rawItemsToDialogue] Dialogue block has no usable sentences."

  forM_ visuals $ \visual ->
    case visual.sentenceOrd of
      Nothing ->
        pure ()
      Just idx ->
        when (idx < 1 || idx > fromIntegral (length sentenceList)) $
          Left $ "@[rawItemsToDialogue] Visual index " <> show idx <> " is out of range for dialogue with "
                  <> show (length sentenceList) <> " sentences."

  pure $
    DialogueBlock
      { emotion = normalizeInline emotionText
      , sentences = sentenceList
      , visuals = visuals
      }

parseEmotionLine :: Text -> Either String (Text, Text)
parseEmotionLine line =
  first errorBundlePretty $
    runParser emotionLineP "<dialogue-first-line>" line

emotionLineP :: NarrationParser (Text, Text)
emotionLineP = do
  hspace
  void $ char '['
  emotionText <- takeWhile1P (Just "emotion annotation") (\c -> c /= ']' && c /= '\n' && c /= '\r')
  void $ char ']'
  hspace1
  sentenceText <- takeWhileP (Just "first sentence") (\c -> c /= '\n' && c /= '\r')
  eof

  let sentenceText' = normalizeInline sentenceText
  when (T.null sentenceText') $
    fail "@[parseEmotionLine] The emotion-prefixed first sentence is empty."

  pure (emotionText, sentenceText')

-- Current deterministic rule:
-- every non-empty spoken line after the first emotion-prefixed line becomes one
-- sentence. If you later want prose-wrapped paragraphs, replace this function
-- with a sentence splitter.
splitContentLine :: Text -> [Text]
splitContentLine line =
  let
    line' = normalizeInline line
  in
  if T.null line' then [] else [line']

normalizeInline :: Text -> Text
normalizeInline =
  T.unwords . T.words

isAsciiDigit :: Char -> Bool
isAsciiDigit = isDigit

tshow :: Show a => a -> Text
tshow = T.pack . show

--------------------------------------------------------------------------------
-- SQL statements
--
-- Assumed schema:
--
--   pitcher.narration(
--     uid bigint primary key generated always as identity,
--     slug text unique not null,
--     title text not null,
--     language text not null,
--     speaker text null
--   )
--
--   pitcher.dialogue(
--     uid bigint primary key generated always as identity,
--     narration_fk bigint not null references pitcher.narration(uid),
--     ord int not null,
--     emotion text not null
--   )
--
--   pitcher.dialogue_sentence(
--     uid bigint primary key generated always as identity,
--     dialogue_fk bigint not null references pitcher.dialogue(uid),
--     ord int not null,
--     body text not null
--   )
--
--   pitcher.dialogue_visual(
--     uid bigint primary key generated always as identity,
--     dialogue_fk bigint not null references pitcher.dialogue(uid),
--     sentence_ord int null,
--     body text not null
--   )

upsertKeynoteStmt :: Statement (Text, Text, Text, Maybe Text) Int64
upsertKeynoteStmt =
  [TH.singletonStatement|
    insert into prod.narration
      (slug, title, language, speaker)
    values
      ($1::text, $2::text, $3::text, $4::text?)
    on conflict (slug) do update
      set title = excluded.title,
          language = excluded.language,
          speaker = excluded.speaker
    returning uid :: int8
  |]

deleteDialogueTreeStmt :: Statement Int64 ()
deleteDialogueTreeStmt =
  [TH.resultlessStatement|
    with target as (
      select uid
      from prod.dialogue
      where narration_fk = $1::int8
    ),
    del_visual as (
      delete from prod.dialogue_visual
      where dialogue_fk in (select uid from target)
    ),
    del_sentence as (
      delete from prod.dialogue_sentence
      where dialogue_fk in (select uid from target)
    )
    delete from prod.dialogue
    where uid in (select uid from target)
  |]

insertDialogueStmt :: Statement (Int64, Int32, Text) Int64
insertDialogueStmt =
  [TH.singletonStatement|
    insert into prod.dialogue
      (narration_fk, ord, emotion)
    values
      ($1::int8, $2::int4, $3::text)
    returning uid :: int8
  |]

insertDialogueSentenceStmt :: Statement (Int64, Int32, Text) Int64
insertDialogueSentenceStmt =
  [TH.singletonStatement|
    insert into prod.dialogue_sentence
      (dialogue_fk, ord, body)
    values
      ($1::int8, $2::int4, $3::text)
    returning uid :: int8
  |]

insertDialogueVisualStmt :: Statement (Int64, Int32,  Int32, Text) Int64
insertDialogueVisualStmt =
  [TH.singletonStatement|
    insert into prod.dialogue_visual
      (dialogue_fk, ord, sentence_ord, body)
    values
      ($1::int8, $2::int4, $3::int4, $4::text)
    returning uid :: int8
  |]