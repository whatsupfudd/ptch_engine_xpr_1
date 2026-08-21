{-# LANGUAGE DerivingStrategies #-}

module Options.Cli where

import Data.Int (Int32, Int64)
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
  , refID :: NarrationIdOpt
  , title :: Text
  , language :: Text
  , speaker :: Maybe Text
  , validateOnly :: Bool
  }
  deriving (Eq, Show)


data NarrationIdOpt =
  EidNI Text
  | NameNI Text
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
  | PublishCmd PublishOpts
  | ProduceCmd ProduceOpts
  | WorkCmd WorkOpts
  | ListCmd ListOpts
  | ExportCmd ExportOpts
  | YouTubeCmd YouTubeOpts
  -- Deprecated:
  -- | LaunchCmd LaunchOpts
  deriving stock (Show)

newtype LaunchOpts = LaunchOpts { 
    jobUid :: String
  }
  deriving (Eq, Show)


data PublishOpts = PublishOpts {
    prodID :: Text
  , titleVideo :: Maybe Text
  , descriptionVideo :: Text
  , tagsVideo :: [Text]
  , categoryID :: Text
  , privacyVideo :: Text
  , notifySubscribers :: Bool
  , madeForKids :: Maybe Bool
  , syntheticMedia :: Maybe Bool
  }
  deriving (Eq, Show)


newtype ProduceOpts = ProduceOpts { 
    narrationId :: NarrationIdOpt
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

data ListOpts = ListOpts { 
    target :: Maybe NarrationIdOpt
    , filter :: Maybe FilterSubCmd
  }
  deriving (Eq, Show)

data ExportOpts = ExportOpts { 
    prodID :: Text
  , outputPath :: FilePath
  }
  deriving (Eq, Show)

newtype YouTubeOpts = YouTubeOpts
  { commandYoutube :: YouTubeSubCmd
  }
  deriving (Eq, Show)


newtype YouTubeSubCmd =
  AuthorizeYouTubeCmd YouTubeAuthorizeOpts
  deriving (Eq, Show)


data YouTubeAuthorizeOpts = YouTubeAuthorizeOpts
  { pathTokenRefresh :: Maybe FilePath
  , noBrowser :: Bool
  , timeoutSeconds :: Int
  , loginHint :: Maybe Text
  }
  deriving (Eq, Show)


data FilterSubCmd =
  DialogueFC
  | RenderNodeFC (Maybe Text) (Maybe Text) (Maybe Int64)
  | RenderJobFC
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
      , ("publish", PublishCmd <$> publishOptsP, "Publishes a render job to a video site.")
      , ("produce", ProduceCmd <$> produceOptsP, "Produces a render job.")
      , ("work", WorkCmd <$> workOptsP, "Works a render job.")
      , ("list", ListCmd <$> listCmdP, "Lists narrations.")
      , ("export", ExportCmd <$> exportOptsP, "Exports a production.")
      , ("youtube", YouTubeCmd <$> youtubeOptsP, "YouTube account and authorization operations.")
      -- Deprecated:
      -- , ("launch", LaunchCmd <$> launchOptsP, "Launches a render job.")
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
          metavar "PATH" <> help "Narration text file to ingest."
        )
    <*> narrationIdOptsP
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


narrationIdOptsP :: Parser NarrationIdOpt
narrationIdOptsP =
  EidNI <$> strOption ( long "eid" <> metavar "EID" <> help "EID for the existing narration (update)." )
  <|> NameNI <$> strOption ( long "name" <> metavar "NAME" <> help "Nickname to refer the narration as." )
  

launchOptsP :: Parser LaunchOpts
launchOptsP =
  LaunchOpts <$> strArgument ( metavar "JOB-UID" <> help "UUID of the job to launch." )
  
publishOptsP :: Parser PublishOpts
publishOptsP =
  PublishOpts
    <$> strArgument (  metavar "PRODUCTION-EID" <> help "EID of the completed production/render job." )
    <*> optional (textOption ( long "title" <> metavar "TITLE" <> help "YouTube title; defaults to the narration title."))
    <*> textOption ( long "description" <> metavar "DESCRIPTION" <> value "" <> help "YouTube video description.")
    <*> many (textOption (  long "tag" <> metavar "TAG" <> help "YouTube tag; may be specified repeatedly."))
    <*> textOption (  long "category" <> metavar "CATEGORY-ID" <> value "22" <> showDefault <> help "YouTube video category ID." )
    <*> textOption (  long "privacy" <> metavar "PRIVATE|UNLISTED|PUBLIC" <> value "private" <> showDefault <> help "YouTube privacy status." )
    <*> switch (  long "notify-subscribers" <> help "Notify channel subscribers about the new video." )
    <*> audienceP
    <*> syntheticMediaP


audienceP :: Parser (Maybe Bool)
audienceP =
  optional $ flag' True (long "made-for-kids" <> help "Declare the video made for kids.")
    <|> flag' False (long "not-made-for-kids" <> help "Declare the video not made for kids.")


syntheticMediaP :: Parser (Maybe Bool)
syntheticMediaP =
  optional $ flag' True (  long "contains-synthetic-media" <> help "Declare realistic altered or synthetic media." )
    <|> flag' False (  long "no-synthetic-media" <> help "Declare that the video does not contain such media." )

produceOptsP :: Parser ProduceOpts
produceOptsP =
  ProduceOpts <$> narrationIdOptsP
  -- strArgument ( metavar "NARRATION-UID" <> help "UUID of the narration to produce." )

workOptsP :: Parser WorkOpts
workOptsP =
  WorkOpts
    <$> textOption ( long "owner" <> metavar "OWNER" <> help "Owner of the worker." )
    <*> textOption ( long "lane" <> metavar "LANE" <> help "Lane of the worker." )
    <*> switch ( long "has-gpu" <> help "Whether the worker has a GPU." )
    <*> optional ( option auto ( long "vram-mb" <> metavar "VRAM-MB" <> help "Amount of VRAM in MB." ) )
    <*> option auto ( long "lease-seconds" <> metavar "LEASE-SECONDS" <> help "Lease seconds." )

listCmdP :: Parser ListOpts
listCmdP =
  ListOpts
    <$> optional narrationIdOptsP
    <*> optional ( subparser (
          command "dialogues" ( info (helper <*> pure DialogueFC) (progDesc "Filter by dialogue.") )
          <> command "rnode" ( info (helper <*> renderNodeFilterP) (progDesc "Filter by render node.") )
          <> command "prod" ( info (helper <*> pure RenderJobFC) (progDesc "Filter by render node.") )
        )
      )


renderNodeFilterP :: Parser FilterSubCmd
renderNodeFilterP =
  RenderNodeFC
    <$> optional ( strOption ( long "lane" <> metavar "LANE" <> help "Lane." ) )
    <*> optional ( strOption ( long "status" <> metavar "STATUS" <> help "Status." ) )
    <*> optional ( option auto ( long "job" <> metavar "JOB-UID" <> help "Job UID." ) )


exportOptsP :: Parser ExportOpts
exportOptsP =
  ExportOpts
    <$> strArgument (  metavar "PRODUCTION-ID" <> help "UUID of the render job to export." )
    <*> strArgument (  metavar "PATH" <> help "Destination path for the finalized video." )


youtubeOptsP :: Parser YouTubeOpts
youtubeOptsP =
  YouTubeOpts <$> hsubparser (command "authorize"
        (info (helper <*> (AuthorizeYouTubeCmd <$> youtubeAuthorizeOptsP))
        (progDesc "Authorize Narravid to upload videos to YouTube."))
      )


youtubeAuthorizeOptsP :: Parser YouTubeAuthorizeOpts
youtubeAuthorizeOptsP =
  YouTubeAuthorizeOpts
    <$> optional
          (strOption
            (  long "token-file"
            <> metavar "PATH"
            <> help
                "Write the refresh token here; overrides youtube.refreshTokenFile."
            )
          )
    <*> switch
          (  long "no-browser"
          <> help
              "Do not launch the browser automatically; print the authorization URL."
          )
    <*> option auto
          (  long "timeout-seconds"
          <> metavar "SECONDS"
          <> value 300
          <> showDefault
          <> help "Time to wait for the local OAuth callback."
          )
    <*> optional
          (textOption
            (  long "login-hint"
            <> metavar "ACCOUNT"
            <> help "Google account email to suggest during authorization."
            )
          )