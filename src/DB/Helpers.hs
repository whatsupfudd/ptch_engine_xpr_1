module DB.Helpers where

import Control.Exception (throwIO)

import Hasql.Pool (Pool, use)
import Hasql.Session (Session)
import Hasql.Transaction (Transaction)
import qualified Hasql.Transaction.Sessions as HTS

runSessionOrThrow :: String -> Pool -> Session a -> IO a
runSessionOrThrow label pool sess = do
  res <- use pool sess
  case res of
    Left err -> throwIO . userError $ "@[" <> label <> "] DB session failed: " <> show err
    Right val -> pure val


runTx :: String -> Pool -> Transaction a -> IO a
runTx label pool tx =
  runSessionOrThrow label pool $ HTS.transaction HTS.Serializable HTS.Write tx
