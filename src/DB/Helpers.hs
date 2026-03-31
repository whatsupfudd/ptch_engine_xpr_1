module DB.Helpers where

import Control.Exception (throwIO)

import Hasql.Pool (Pool, use)
import Hasql.Session (Session)
import Hasql.Transaction (Transaction)
import qualified Hasql.Transaction.Sessions as HTS

runSessionOrThrow :: Pool -> Session a -> IO a
runSessionOrThrow pool sess = do
  res <- use pool sess
  case res of
    Left err -> throwIO . userError $ "DB session failed: " <> show err
    Right val -> pure val


runTx :: Pool -> Transaction a -> IO a
runTx pool tx =
  runSessionOrThrow pool $ HTS.transaction HTS.Serializable HTS.Write tx
