{-# LANGUAGE DerivingStrategies #-}

module Options.Cli where

import Data.Int (Int32)
import Data.Text (Text, pack)
import Data.UUID (UUID, fromString)
import Options.Applicative


newtype EnvOptions = EnvOptions {
    appHome :: Maybe FilePath
  }

data CliOptions = CliOptions {
  debug :: Maybe Int
  , configFile :: Maybe FilePath
  , job :: Maybe Command
 }
 deriving stock (Show)

data IngestOpts = IngestOpts
  { inputPath :: FilePath
  , slug :: Text
  , title :: Text
  , language :: Text
  , speaker :: Maybe Text
  , validateOnly :: Bool
  }
  deriving (Eq, Show)


textOption :: Mod OptionFields String -> Parser Text
textOption mods =
  pack <$> strOption mods


data GlobalOptions = GlobalOptions {
  confPathGO :: String
  , debugGO :: String
  }

data Command =
  HelpCmd
  | VersionCmd
  | IngestCmd IngestOpts
  | LaunchCmd LaunchOpts
  | PublishCmd PublishOpts
  | ProduceCmd ProduceOpts
  | WorkCmd WorkOpts
  deriving stock (Show)

newtype LaunchOpts = LaunchOpts { 
    jobUid :: String
  }
  deriving (Eq, Show)

newtype PublishOpts = PublishOpts { 
    jobUid :: String
  }
  deriving (Eq, Show)

newtype ProduceOpts = ProduceOpts { 
    jobUid :: String
  }
  deriving (Eq, Show)

data WorkOpts = WorkOpts { 
    owner :: Text
  , lane :: Text
  , hasGpu :: Bool
  , vramMb :: Maybe Int32
  , leaseSeconds :: Int32
  }
  deriving (Eq, Show)

parseCliOptions :: IO (Either String CliOptions)
parseCliOptions =
  Right <$> execParser parser

parser :: ParserInfo CliOptions
parser =
  info (helper <*> argumentsP) $
    fullDesc <> progDesc "narravid." <> header "narravid - ."


argumentsP :: Parser CliOptions
argumentsP = do
  buildOptions <$> globConfFileDef <*> hsubparser commandDefs
  where
    buildOptions :: GlobalOptions -> Command -> CliOptions
    buildOptions globs cmd =
      let
        mbConfPath = case globs.confPathGO of
          "" -> Nothing
          aValue -> Just aValue
        mbDebug = case globs.debugGO of
          "" -> Nothing
          aValue -> Just (read aValue :: Int)
      in
      CliOptions {
        debug = mbDebug
        , configFile = mbConfPath
        , job = Just cmd
      }


globConfFileDef :: Parser GlobalOptions
globConfFileDef =
  GlobalOptions <$>
    strOption (
      long "config"
      <> short 'c'
      <> metavar "narravidCONF"
      <> value ""
      <> showDefault
      <> help "Global config file (default is ~/.narravid/config.yaml)."
    )
    <*>
    strOption (
      long "debug"
      <> short 'd'
      <> metavar "DEBUGLVL"
      <> value ""
      <> showDefault
      <> help "Global debug state."
    )


commandDefs :: Mod CommandFields Command
commandDefs =
  let
    cmdArray = [
      ("help", pure HelpCmd, "Help about any command.")
      , ("version", pure VersionCmd, "Shows the version number of importer.")
      , ("ingest", IngestCmd <$> ingestOptsP, "Ingests a narration text file into the database.")
      , ("launch", LaunchCmd <$> launchOptsP, "Launches a render job.")
      , ("publish", PublishCmd <$> publishOptsP, "Publishes a render job to a video site.")
      , ("produce", ProduceCmd <$> produceOptsP, "Produces a render job.")
      , ("work", WorkCmd <$> workOptsP, "Works a render job.")
      ]
    headArray = head cmdArray
    tailArray = tail cmdArray
  in
    foldl (\accum aCmd -> (cmdBuilder aCmd) <> accum) (cmdBuilder headArray) tailArray
  where
    cmdBuilder (label, cmdDef, desc) =
      command label (info cmdDef (progDesc desc))


ingestOptsP :: Parser IngestOpts
ingestOptsP =
  IngestOpts
    <$> strArgument (
          metavar "PATH" <> help "UTF-8 narration text file to ingest."
        )
    <*> strArgument (
           metavar "LABEL"
          <> help "Stable narration label used as the Pitcher identity key."
          )
    <*> textOption
          (  long "title"
          <> metavar "NARRATION_TITLE"
          <> help "Human-readable narration title."
          )
    <*> textOption
          (  long "lang"
          <> metavar "LANG"
          <> value "en"
          <> showDefault
          <> help "Language code, for example en, en-US, ar."
          )
    <*> optional
          (textOption
            (  long "speaker"
            <> metavar "SPEAKER_NAME"
            <> help "Optional speaker name."
            )
          )
    <*> switch
          (  long "validate-only"
          <> help "Parse and validate the narration without writing to the database."
          )


launchOptsP :: Parser LaunchOpts
launchOptsP =
  LaunchOpts <$> strArgument ( metavar "JOB-UID" <> help "UUID of the job to launch." )
  
publishOptsP :: Parser PublishOpts
publishOptsP =
  PublishOpts <$> strArgument ( metavar "JOB-UID" <> help "UUID of the job to publish." )

produceOptsP :: Parser ProduceOpts
produceOptsP =
  ProduceOpts <$> strArgument ( metavar "JOB-UID" <> help "UUID of the job to produce." )

workOptsP :: Parser WorkOpts
workOptsP =
  WorkOpts
    <$> textOption ( long "owner" <> metavar "OWNER" <> help "Owner of the worker." )
    <*> textOption ( long "lane" <> metavar "LANE" <> help "Lane of the worker." )
    <*> switch ( long "has-gpu" <> help "Whether the worker has a GPU." )
    <*> optional ( option auto ( long "vram-mb" <> metavar "VRAM-MB" <> help "Amount of VRAM in MB." ) )
    <*> option auto ( long "lease-seconds" <> metavar "LEASE-SECONDS" <> help "Lease seconds." )