module Pitcher.Ingest.Parser
  ( NarrationParser
  , narrationP
  , dialogueP
  , sentenceP
  , emotionsP
  , visualsP
  ) where

import Control.Monad (void, when)
import Data.Char (isDigit)
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec
  , anySingle
  , choice
  , eof
  , lookAhead
  , many
  , manyTill
  , notFollowedBy
  , optional
  , some
  , takeWhile1P
  , try
  , (<|>)
  )
import Text.Megaparsec.Char
  ( char
  , eol
  , string
  )

--------------------------------------------------------------------------------
-- Assumed AST
--
-- Adapt the constructors/field names here if your local types differ.

import Pitcher.NarrationTypes (Narration(..), Dialogue(..), DialogueVisual(..))
import Control.Applicative (asum)

--------------------------------------------------------------------------------
-- Parser type

type NarrationParser = Parsec Void Text

--------------------------------------------------------------------------------
-- Top-level narration parser

narrationP :: NarrationParser Narration
narrationP = do
  skipIgnorableAllP
  ds <- some (dialogueP <* skipIgnorableAllP)
  eof
  pure Narration { dialogues = ds }


dialogueP :: NarrationParser Dialogue
dialogueP = do
  dialogueHeaderP
  headerGapP
  emos <- fromMaybe [] <$> optional (try emotionsP)
  sents <- some dialogueSentenceP
  vis <- asum [
      try visualsP
      , visualBlockGapP >> pure []
    ]
  pure Dialogue { emotions = emos, sentences = sents, visuals = vis }


dialogueHeaderP :: NarrationParser ()
dialogueHeaderP = do
  skipInlineStuffP
  void $ string "[dialogue]"

--------------------------------------------------------------------------------
-- Sentences

dialogueSentenceP :: NarrationParser Text
dialogueSentenceP = do
  notFollowedByAt dialogueSentenceStopStartP
  sentenceP dialogueSentenceStopStartP sentenceBoundaryDialogueP

visualSentenceP :: NarrationParser Text
visualSentenceP = do
  notFollowedByAt visualSentenceStopStartP
  sentenceP visualSentenceStopStartP sentenceBoundaryVisualP

sentenceP :: NarrationParser () -> NarrationParser () -> NarrationParser Text
sentenceP stopStartP boundaryP = do
  skipInlineStuffP
  notFollowedByAt stopStartP

  body <- manyTill sentenceBodyCharP (lookAhead sentenceEndP)

  let bodyTxt = normalizeInline (T.pack body)

  when (T.null bodyTxt) $
    fail "Empty sentence body."

  ending <- sentenceEndP
  boundaryP

  pure (bodyTxt <> ending)
  where
    sentenceBodyCharP = do
      notFollowedByAt stopStartP
      anySingle

sentenceEndP :: NarrationParser Text
sentenceEndP =
      "..." <$ try (string "...")
  <|> "."   <$ char '.'
  <|> "!"   <$ char '!'
  <|> "?"   <$ char '?'
  <|> ":"   <$ char ':'

sentenceBoundaryDialogueP :: NarrationParser ()
sentenceBoundaryDialogueP =
      void sentenceGapP
  <|> eof


sentenceBoundaryVisualP :: NarrationParser ()
sentenceBoundaryVisualP =
      immediateVisualCloseP
  <|> rejectInlineSpaceBeforeVisualCloseP
  <|> void sentenceGapP
  <|> eof

immediateVisualCloseP :: NarrationParser ()
immediateVisualCloseP =
  lookAhead $ void (char ']')

rejectInlineSpaceBeforeVisualCloseP :: NarrationParser ()
rejectInlineSpaceBeforeVisualCloseP = try $ do
  inlineSpace1P
  lookAhead $ void (char ']')
  fail "The closing ']' of a visuals block must either immediately follow the sentence terminator or appear on a separate line."


dialogueSentenceStopStartP :: NarrationParser ()
dialogueSentenceStopStartP =
      try visualStartP
  <|> try dialogueHeaderStartP

visualSentenceStopStartP :: NarrationParser ()
visualSentenceStopStartP =
      try visualCloseStartP
  <|> try dialogueHeaderStartP
  <|> try visualStartP

visualCloseStartP :: NarrationParser ()
visualCloseStartP = do
  skipInlineStuffP
  void $ char ']'

dialogueHeaderStartP :: NarrationParser ()
dialogueHeaderStartP = do
  skipInlineStuffP
  void $ string "[dialogue]"

visualStartP :: NarrationParser ()
visualStartP = do
  skipInlineStuffP
  void $ char '['
  skipInlineStuffP
  void $ string "visuals"

notFollowedByAt :: NarrationParser () -> NarrationParser ()
notFollowedByAt p =
  notFollowedBy (try p)

--------------------------------------------------------------------------------
-- Emotions

emotionsP :: NarrationParser [Text]
emotionsP = try $ do
  skipInlineStuffP
  void $ char '['
  skipInlineStuffP

  notFollowedBy (string "dialogue")
  notFollowedBy (string "visuals")

  firstEmotion <- emotionItemP
  rest <- many $ do
    skipInlineStuffP
    void $ char ','
    skipInlineStuffP
    emotionItemP

  skipInlineStuffP
  void $ char ']'
  skipInlineStuffP

  pure (firstEmotion : rest)

emotionItemP :: NarrationParser Text
emotionItemP = do
  raw <- takeWhile1P
    (Just "emotion item")
    (\c -> c /= ',' && c /= ']' && c /= '\n' && c /= '\r')
  let txt = normalizeInline raw
  when (T.null txt) $
    fail "Empty emotion item."
  pure txt

--------------------------------------------------------------------------------
-- Visuals
--
-- Practical interpretation:
--   * single visual  => one [visuals: ...] block
--   * multiple visuals => one or more [visuals(n): ...] blocks
--
-- This matches the original examples better than restricting the indexed form
-- to exactly one block.

visualsP :: NarrationParser [DialogueVisual]
visualsP = do
  visualsHeaderP
  asum [
    do
      void $ char ':'
      sents <- some visualSentenceP
      visualBlockCloseP
      pure [DialogueVisual { sentenceOrd = Nothing, description = renderSentenceBlock sents }]
    , do
      n <- getIndexP
      sents <- some visualSentenceP
      visualBlockCloseP
      rest <- many $ try $ do
        visualBlockGapP
        visualsHeaderP
        n <- getIndexP
        sents <- some visualSentenceP
        visualBlockCloseP
        pure DialogueVisual { sentenceOrd = Just n, description = renderSentenceBlock sents }
      pure (DialogueVisual { sentenceOrd = Just n, description = renderSentenceBlock sents } : rest)
    ]

visualsHeaderP :: NarrationParser ()
visualsHeaderP = do
  void $ char '['
  skipInlineStuffP
  void $ string "visuals"


getIndexP :: NarrationParser Int32
getIndexP = do
  void $ char '('
  n <- fromIntegral <$> decimalP
  void $ char ')'
  void $ char ':'
  pure n


indexedVisualP :: NarrationParser DialogueVisual
indexedVisualP = do
  -- skipIgnorableAllP
  ix <- indexedVisualHeaderP
  sents <- some visualSentenceP
  visualBlockCloseP
  pure DialogueVisual { sentenceOrd = Just ix, description = renderSentenceBlock sents }


indexedVisualHeaderP :: NarrationParser Int32
indexedVisualHeaderP = do
  void $ char '['
  skipInlineStuffP
  void $ string "visuals("
  n <- fromIntegral <$> decimalP
  void $ string "):"
  skipInlineStuffP
  pure n


singleVisualP :: NarrationParser [DialogueVisual]
singleVisualP = do
  skipIgnorableAllP
  singleVisualHeaderP
  sents <- some visualSentenceP
  visualBlockCloseP
  pure [ DialogueVisual { sentenceOrd = Nothing, description = renderSentenceBlock sents } ]


multipleVisualsP :: NarrationParser [DialogueVisual]
multipleVisualsP = do
  firstVisual <- indexedVisualP
  rest <- many $ do
    visualBlockGapP
    indexedVisualP
  pure (firstVisual : rest)


singleVisualHeaderP :: NarrationParser ()
singleVisualHeaderP = do
  void $ char '['
  skipInlineStuffP
  void $ string "visuals:"
  skipInlineStuffP


visualBlockCloseP :: NarrationParser ()
visualBlockCloseP = do
  skipInlineStuffP
  void $ char ']'


--------------------------------------------------------------------------------
-- Whitespace / comments
--
-- These helpers are deliberately split so that:
--   * some/many only wrap input-consuming parsers
--   * newline-sensitive places really require newline consumption

skipIgnorableAllP :: NarrationParser ()
skipIgnorableAllP =
  void $ many ignorableChunkP

skipInlineStuffP :: NarrationParser ()
skipInlineStuffP =
  void $ many inlineChunkP

headerGapP :: NarrationParser ()
headerGapP = do
  void $ some newlineChunkP
  skipInlineStuffP

sentenceGapP :: NarrationParser ()
sentenceGapP =
  void $ some sentenceGapChunkP


ignorableChunkP :: NarrationParser ()
ignorableChunkP =
      try newlineChunkP
  <|> try blockCommentP
  <|> try lineCommentAtEofP
  <|> inlineChunkP

inlineChunkP :: NarrationParser ()
inlineChunkP =
      inlineSpace1P
  <|> try blockCommentP


visualBlockGapP :: NarrationParser ()
visualBlockGapP =
  void $ some visualGapChunkP

sentenceGapChunkP :: NarrationParser ()
sentenceGapChunkP =
      try newlineChunkP
  <|> try blockCommentP
  <|> try lineCommentAtEofP
  <|> inlineSpace1P

visualGapChunkP :: NarrationParser ()
visualGapChunkP =
      try newlineChunkP
  <|> try blockCommentP
  <|> try lineCommentAtEofP
  <|> inlineSpace1P

inlineSpace1P :: NarrationParser ()
inlineSpace1P = do
  void $
    takeWhile1P
      (Just "inline space")
      (\c -> c == ' ' || c == '\t')

newlineChunkP :: NarrationParser ()
newlineChunkP = do
  skipInlineStuffP
  optional lineCommentNoEolP
  void eol

lineCommentNoEolP :: NarrationParser ()
lineCommentNoEolP = do
  void $ string "--"
  void $ manyTill anySingle (lookAhead (void eol <|> eof))

lineCommentAtEofP :: NarrationParser ()
lineCommentAtEofP = do
  lineCommentNoEolP
  eof

blockCommentP :: NarrationParser ()
blockCommentP = do
  void $ string "{-"
  void $ manyTill anySingle (try (string "-}"))


--------------------------------------------------------------------------------
-- Small helpers


decimalP :: NarrationParser Int
decimalP = do
  digits <- takeWhile1P (Just "number") isDigit
  pure (read (T.unpack digits))


normalizeInline :: Text -> Text
normalizeInline =
  T.unwords . T.words

renderSentenceBlock :: [Text] -> Text
renderSentenceBlock =
  T.intercalate " "

