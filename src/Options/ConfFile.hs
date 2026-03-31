{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Options.ConfFile where

import qualified Control.Exception as Cexc
import qualified Data.Aeson as Aes
import Data.Text (Text)
import GHC.Generics
import qualified System.Directory as Sdir
import qualified System.Environment as Senv
import qualified System.FilePath.Posix as Spsx
import qualified System.IO.Error as Serr

import qualified Data.Yaml as Yaml


data PgDbOpts = PgDbOpts {
  port :: Maybe Int
  , host :: Maybe String
  , user :: Maybe String
  , passwd :: Maybe String
  , dbase :: Maybe String
  , poolSize :: Maybe Int
  , poolTimeOut :: Maybe Int
}
  deriving stock (Show, Generic)

data S3Options = S3Options {
  accessKey :: Maybe Text
  , secretKey :: Maybe Text
  , host :: Maybe Text
  , region :: Maybe Text
  , bucket :: Maybe Text
  }
  deriving stock (Show, Generic)

data AiOptions = AiOptions {
  server :: Maybe Text
  , user :: Maybe Text
  , password :: Maybe Text
  , ttsFunctionEid :: Maybe String
  , ttsSpeaker :: Maybe Text
  , imageFunctionEid :: Maybe String
  , imageModel :: Maybe Text
  }
  deriving stock (Show, Generic)

data FileOptions = FileOptions {
  debug :: Maybe Int
  , primaryLocale :: Maybe String
  , pgDb :: Maybe PgDbOpts
  , s3store :: Maybe S3Options
  , ai :: Maybe AiOptions
 }
 deriving stock (Show, Generic)


defaultConfName :: FilePath
-- HERE: modify config file to .config/narravid.yaml or .narravid/config.yaml
defaultConfName = ".fudd/narravid/config.yaml"


defaultConfigFilePath :: IO (Either String FilePath)
defaultConfigFilePath = do
  eiHomeDir <- Cexc.try $ Sdir.getHomeDirectory :: IO (Either Serr.IOError FilePath)
  case eiHomeDir of
    Left err -> pure . Left $ "@[defaultConfigFilePath] err: " <> show err
    Right aPath -> pure . Right $ Spsx.joinPath [aPath, defaultConfName]


-- YAML support:
instance Aes.FromJSON FileOptions
instance Aes.FromJSON PgDbOpts
instance Aes.FromJSON S3Options
instance Aes.FromJSON AiOptions


parseFileOptions :: FilePath -> IO (Either String FileOptions)
parseFileOptions filePath =
  let
    fileExt = Spsx.takeExtension filePath
  in case fileExt of
    ".yaml" -> do
      eiRez <- Yaml.decodeFileEither filePath
      case eiRez of
        Left err -> pure . Left $ "@[parseYaml] err: " <> show err
        Right aContent ->
          {- HERE: add new parameters processing:
          Eg:
          case aContent.rootDir of
            Nothing -> pure $ Right aContent
            Just aVal -> case head aVal of
                '$' -> do
                  eiEnvValue <- Cexc.try $ Senv.getEnv (tail aVal) :: IO (Either Serr.IOError String)
                  case eiEnvValue of
                    Left err -> pure . Right $ aContent { rootDir = Nothing }
                    Right aVal -> pure . Right $ aContent { rootDir = Just aVal }
                _ -> pure . Right $ aContent { rootDir = Just aVal }
          -}
          -- HERE: modify based on new parameters processing:
          pure $ Right aContent
    _ -> pure . Left $ "@[parseFileOptions] unknown conf-file extension: " <> fileExt
