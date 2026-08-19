module Commands.Export (exportCmd) where

import qualified Control.Monad.Cont as Mc
import Control.Exception (throwIO)

import qualified Data.UUID as Uu

import Hasql.Pool (Pool)
import Hasql.Session (statement)

import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

import Assets.S3Ops (downloadAssetToPath, makeS3Conn)
import Assets.Types (S3Conn)
import DB.Connect (startPg)
import DB.Helpers (runSessionOrThrow)
import qualified DB.ProducerStmt as Ps
import Options.Cli (ExportOpts (..))
import Options.Runtime (RunOptions (..))


exportCmd :: ExportOpts -> RunOptions -> IO ()
exportCmd opts rtOpts =
  let
    pgPool = startPg rtOpts.pgDbConf
    s3Conn = makeS3Conn rtOpts.s3store
  in
  Mc.runContT pgPool $ exportProduction opts s3Conn


exportProduction :: ExportOpts -> S3Conn -> Pool -> IO ()
exportProduction opts s3Conn pool = 
  case Uu.fromText opts.prodID of
    Nothing -> throwIO . userError $ "@[exportProduction] invalid production ID: " <> show opts.prodID
    Just prodID -> do
      mbRenderID <- runSessionOrThrow "selectProductionUidStmt" pool $ statement prodID Ps.selectRenderJobByEid
      case mbRenderID of
        Nothing -> throwIO . userError $ "@[exportProduction] production " <> show opts.prodID <> " not found."
        Just renderID -> do
          mbAsset <- runSessionOrThrow "selectFinalAssetStmt" pool $ statement renderID Ps.selectFinalAssetStmt
          case mbAsset of
            Nothing -> throwIO . userError $ "@[exportProduction] production " <> show opts.prodID
                  <> " is not completed or has no final asset."
            Just (_, eidAsset) -> do
              createDirectoryIfMissing True $ takeDirectory opts.outputPath
              downloadAssetToPath s3Conn eidAsset opts.outputPath
              putStrLn $ "Exported production " <> show opts.prodID <> " to " <> opts.outputPath