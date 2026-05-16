module Commands.Publish (publishCmd) where

import qualified Control.Monad.Cont as Mc

import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID, fromString)

import Hasql.Pool (Pool)

import DB.Connect (startPg)
import Assets.S3Ops (makeS3Conn)
import Options.Cli (PublishOpts (..), NarrationIdOpt (..))
import Options.Runtime (RunOptions (..), PgDbConfig (..), AiConfig (..))
import Pitcher.Render.Types (RenderOutcome (..))

publishCmd :: PublishOpts -> RunOptions -> IO ()
publishCmd opts rtOpts =
  let
    env = 1
    pgPool = startPg rtOpts.pgDbConf
  in do
  Mc.runContT pgPool (launchJob opts.narrationId)

launchJob :: NarrationIdOpt -> Pool -> IO ()
launchJob narrTarget pool = do
  putStrLn $ "Publishing narration " <> show narrTarget
