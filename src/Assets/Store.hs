module Assets.Store where


import qualified Data.ByteString as Bs
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as Uu
import qualified Data.UUID.V4 as Uu4

import System.Directory (getFileSize)

import Hasql.Pool (Pool)

import Assets.Types (S3Conn, AssetRef (..))
import Assets.S3Ops (putBytesObject, putFileObject)
import DB.LaunchOps (insertAssetRow)


insertBytesAsAsset :: Pool -> S3Conn -> Text -> Text -> Text -> Bs.ByteString -> IO AssetRef
insertBytesAsAsset pool s3Conn name contentType notes bytes = do
  putStrLn $ "@[insertBytesAsAsset] inserting asset: " <> T.unpack name
  eid <- Uu4.nextRandom
  putBytesObject s3Conn (Uu.toText eid) bytes
  uid <- insertAssetRow pool name eid contentType (fromIntegral $ Bs.length bytes) notes
  pure AssetRef { uid = uid, eid = eid }


uploadFileAsAsset :: Pool -> S3Conn -> FilePath -> Text -> Text -> Text -> IO AssetRef
uploadFileAsAsset pool s3Conn path name contentType notes = do
  eid <- Uu4.nextRandom
  putFileObject s3Conn path (Uu.toText eid)
  size <- fromIntegral <$> getFileSize path
  uid <- insertAssetRow pool name eid contentType size notes
  pure AssetRef { uid = uid, eid = eid }
