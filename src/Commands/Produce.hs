module Commands.Produce (produceCmd) where

import qualified Control.Monad.Cont as Mc

import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID, fromString)

import Hasql.Pool (Pool)

import DB.Connect (startPg)
import Assets.S3Ops (makeS3Conn)
import Options.Cli (ProduceOpts (..), NarrationIdOpt (..))
import Options.Runtime (RunOptions (..), PgDbConfig (..), AiConfig (..))
import Pitcher.Render.Producer (ProducerCfg (..), TrailingDialoguePolicy(..))
import Pitcher.Render.Types (RenderOutcome (..))
import qualified Pitcher.Render.Producer as Pr

produceCmd :: ProduceOpts -> RunOptions -> IO ()
produceCmd opts rtOpts =
  let
    params = ProducerCfg {
        renderVersionTag = "v1"
      , defaultMaxAttempts = 1
      , finalGapSeconds = 0.5
      , finalFadeSeconds = 0.5
      , ttsVoice = Just "en-US-Standard-A"
      , imageStyleTag = "v1"
      , segmentPolicyTag = "v1"
      , finalPolicyTag = "v1"
      , trailingDialoguePolicy = AttachTrailingToPreviousSection
      }
    pgPool = startPg rtOpts.pgDbConf
  in do
  Mc.runContT pgPool (launchJob params opts.narrationId)

launchJob :: ProducerCfg -> NarrationIdOpt -> Pool -> IO ()
launchJob cfg narrTarget pool = do
  putStrLn $ "@[launchJob] narration: " <> show narrTarget
  graphUid <- Pr.launchProducer cfg pool narrTarget
  putStrLn $ "@[launchJob] render_job UID: " <> show graphUid
