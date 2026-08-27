module Commands.Produce (produceCmd) where

import qualified Control.Monad.Cont as Mc

import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID, fromString)

import Hasql.Pool (Pool)

import DB.Connect (startPg)
import Assets.S3Ops (makeS3Conn)
import Options.Cli (ProduceOpts (..), NarrationIdOpt (..))
import Options.Runtime (RunOptions (..))
import qualified Pitcher.Render.Producer as Pr

produceCmd :: ProduceOpts -> RunOptions -> IO ()
produceCmd opts rtOpts =
  let
    params = Pr.defaultProducerCfg
    pgPool = startPg rtOpts.pgDbConf
  in do
  Mc.runContT pgPool (launchJob params opts.narrationId)

launchJob :: Pr.ProducerCfg -> NarrationIdOpt -> Pool -> IO ()
launchJob cfg narrTarget pool = do
  putStrLn $ "@[launchJob] narration: " <> show narrTarget
  graphUid <- Pr.launchProducer cfg pool narrTarget
  putStrLn $ "@[launchJob] render_job UID: " <> show graphUid
