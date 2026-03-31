module Commands.Publish (publishCmd) where

import qualified Control.Monad.Cont as Mc

import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID, fromString)

import Hasql.Pool (Pool)

import DB.Connect (startPg)
import Assets.S3Ops (makeS3Conn)
import Options.Cli (PublishOpts (..))
import Options.Runtime (RunOptions (..), PgDbConfig (..), AiConfig (..))
import Pitcher.Render.Types (RenderOutcome (..))

publishCmd :: PublishOpts -> RunOptions -> IO ()
publishCmd opts rtOpts = do
  case fromString opts.jobUid of
    Nothing -> error $ "@[publishCmd] invalid job UID: " <> show opts.jobUid
    Just jobEid ->
      let
        env = 1
      in
      let
        pgPool = startPg rtOpts.pgDbConf
      in do
      Mc.runContT pgPool (launchJob jobEid)

launchJob :: UUID -> Pool -> IO ()
launchJob jobEid pool = do
  putStrLn $ "Publishing job " <> show jobEid
