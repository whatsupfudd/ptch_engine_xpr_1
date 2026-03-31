module Options.Runtime (defaultRun, RunOptions (..), PgDbConfig (..), defaultPgDbConf, AiConfig (..), defaultAiConf) where
-- import Data.Int (Int)

import Data.Text (Text)
import Data.UUID (UUID)

import DB.Connect (PgDbConfig (..), defaultPgDbConf)
import Assets.Types (S3Config (..), defaultS3Conf)

data AiConfig = AiConfig {
    server :: String
    , user :: Text
    , password :: Text
    , ttsFunctionEid :: Maybe UUID
    , ttsSpeaker :: Text
    , imageFunctionEid :: Maybe UUID
    , imageModel :: Text
  }
  deriving (Show)

defaultAiConf :: AiConfig
defaultAiConf =
  AiConfig {
    server = "http://localhost:8000"
    , user = "admin"
    , password = "password"
    , ttsFunctionEid = Nothing
    , ttsSpeaker = "default"
    , imageFunctionEid = Nothing
    , imageModel = "default"
  }

data RunOptions = RunOptions {
    debug :: Int
    , pgDbConf :: PgDbConfig
    , s3store :: S3Config
    , aiConf :: AiConfig
  }
  deriving (Show)

defaultRun :: RunOptions
defaultRun =
  RunOptions {
    debug = 0
    , pgDbConf = defaultPgDbConf
    , s3store = defaultS3Conf
    , aiConf = defaultAiConf
  }
