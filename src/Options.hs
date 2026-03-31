module Options  (
  module Cl
  , module Fo
  , module Rt
  , mergeOptions
 )
where

import Control.Monad.State ( MonadState (put), MonadIO, runStateT, State, StateT, modify, lift, liftIO )
import Control.Monad.Except ( ExceptT, MonadError (throwError) )
import Data.Functor.Identity ( Identity (..) )

import Data.Foldable (for_)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.UUID (fromString)

import qualified System.IO.Error as Serr
import qualified Control.Exception as Cexc
import qualified System.Posix.Env as Senv
import qualified System.Directory as Sdir


import qualified Options.Cli as Cl (CliOptions (..), EnvOptions (..))
import qualified Options.ConfFile as Fo (FileOptions (..), PgDbOpts (..), S3Options (..), AiOptions (..))
import qualified Assets.Types as At
import qualified Options.Runtime as Rt

type ConfError = Either String ()
type RunOptSt = State Rt.RunOptions ConfError
type RunOptIOSt = StateT Rt.RunOptions IO ConfError
type PgDbOptIOSt = StateT Rt.PgDbConfig (StateT Rt.RunOptions IO) ConfError
type S3OptIOSt = StateT At.S3Config (StateT Rt.RunOptions IO) ConfError
type AiOptIOSt = StateT Rt.AiConfig (StateT Rt.RunOptions IO) ConfError


mconf :: MonadState s m => Maybe t -> (t -> s -> s) -> m ()
mconf mbOpt setter =
  case mbOpt of
    Nothing -> pure ()
    Just opt -> modify $ setter opt

innerConf :: MonadState s f => (t1 -> s -> s) -> (t2 -> StateT t1 f (Either a b)) -> t1 -> Maybe t2 -> f ()
innerConf updState innerParser defaultVal mbOpt =
  case mbOpt of
    Nothing -> pure ()
    Just anOpt -> do
      (result, updConf) <- runStateT (innerParser anOpt) defaultVal
      case result of
        Left errMsg -> pure ()
        Right _ -> modify $ updState updConf


mergeOptions :: Cl.CliOptions -> Fo.FileOptions -> Cl.EnvOptions -> IO (Either String Rt.RunOptions)
mergeOptions cli file env = do
  (result, runtimeOpts) <- runStateT (parseOptions cli file) Rt.defaultRun
  case result of
    Left errMsg -> pure . Left $ errMsg
    Right _ -> pure . Right $ runtimeOpts
  where
  parseOptions :: Cl.CliOptions -> Fo.FileOptions -> RunOptIOSt
  parseOptions cli file = do
    mconf cli.debug $ \nVal s -> s { Rt.debug = nVal }
    innerConf (\nVal s -> s { Rt.pgDbConf = nVal }) parsePgDb Rt.defaultPgDbConf file.pgDb
    innerConf (\nVal s -> s { Rt.s3store = nVal }) parseS3 At.defaultS3Conf file.s3store
    innerConf (\nVal s -> s { Rt.aiConf = nVal }) parseAi Rt.defaultAiConf file.ai
    pure $ Right ()

  parsePgDb :: Fo.PgDbOpts -> PgDbOptIOSt
  parsePgDb dbO = do
    mconf dbO.host $ \nVal s -> s { Rt.host = T.encodeUtf8 . T.pack $ nVal }
    mconf dbO.port $ \nVal s -> s { Rt.port = fromIntegral nVal }
    mconf dbO.user $ \nVal s -> (s :: Rt.PgDbConfig) { Rt.user = T.encodeUtf8 . T.pack $ nVal }
    mconf dbO.passwd $ \nVal s -> s { Rt.passwd = T.encodeUtf8 . T.pack $ nVal }
    mconf dbO.dbase $ \nVal s -> s { Rt.dbase = T.encodeUtf8 . T.pack $ nVal }
    pure $ Right ()

  parseS3 :: Fo.S3Options -> S3OptIOSt
  parseS3 s3O = do
    mconf s3O.accessKey $ \nVal s -> s { At.user = nVal }
    mconf s3O.secretKey $ \nVal s -> s { At.passwd = nVal }
    mconf s3O.host $ \nVal s -> s { At.host = nVal }
    mconf s3O.region $ \nVal s -> s { At.region = nVal }
    mconf s3O.bucket $ \nVal s -> s { At.bucket = nVal }
    pure $ Right ()

  parseAi :: Fo.AiOptions -> AiOptIOSt
  parseAi aiO = do
    mconf aiO.server $ \nVal s -> s { Rt.server = T.unpack nVal }
    mconf aiO.user $ \nVal s -> (s :: Rt.AiConfig) { Rt.user = nVal }
    mconf aiO.password $ \nVal s -> s { Rt.password = nVal }
    mconf aiO.ttsFunctionEid $ \nVal s -> s { Rt.ttsFunctionEid = fromString nVal }
    mconf aiO.ttsSpeaker $ \nVal s -> s { Rt.ttsSpeaker = nVal }
    mconf aiO.imageFunctionEid $ \nVal s -> s { Rt.imageFunctionEid = fromString nVal }
    mconf aiO.imageModel $ \nVal s -> s { Rt.imageModel = nVal }
    pure $ Right ()

-- | resolveEnvValue resolves an environment variable value.
resolveEnvValue :: FilePath -> IO (Maybe FilePath)
resolveEnvValue aVal =
  case head aVal of
      '$' ->
        let
          (envName, leftOver) = break ('/' ==) aVal
        in do
        mbEnvValue <- Senv.getEnv $ tail envName
        case mbEnvValue of
          Nothing -> pure Nothing
          Just aVal -> pure . Just $ aVal <> leftOver
      _ -> pure $ Just aVal

