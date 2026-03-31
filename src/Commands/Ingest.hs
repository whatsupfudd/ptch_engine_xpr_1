module Commands.Ingest where

import qualified Control.Monad.Cont as Mc

import qualified Options.Runtime as Rto
import Pitcher.Ingest (ingest)
import Options.Cli (IngestOpts (..))
import DB.Connect (startPg)


ingestCmd :: IngestOpts -> Rto.RunOptions -> IO ()
ingestCmd ingestOpts rtOpts =
  let
    pgPool = startPg rtOpts.pgDbConf
  in do
  Mc.runContT pgPool (ingest ingestOpts)