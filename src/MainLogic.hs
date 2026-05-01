module MainLogic
where

import Data.Text (pack)
import qualified System.Environment as Env

import qualified Options as Opt
import qualified Options.Cli as Opt (CliOptions (..), EnvOptions (..), Command (..))
import qualified Options.ConfFile as Opt (FileOptions (..))
import Commands as Cmd


runWithOptions :: Opt.CliOptions -> Opt.FileOptions -> IO ()
runWithOptions cliOptions fileOptions = do
  -- putStrLn $ "@[runWithOptions] cliOpts: " <> show cliOptions
  -- putStrLn $ "@[runWithOptions] fileOpts: " <> show fileOptions
  case cliOptions.job of
    Nothing -> do
      putStrLn "@[runWithOptions] start on nil command."
    Just aJob -> do
      -- Get environmental context in case it's required in the merge. Done here to keep the merge pure:
      mbHome <- Env.lookupEnv "narravidHOME"
      let
        envOptions = Opt.EnvOptions {
            Opt.appHome = mbHome
            -- TODO: put additional env vars.
          }
        -- switchboard to command executors:
        cmdExecutor =
          case aJob of
            Opt.HelpCmd -> Cmd.helpCmd
            Opt.VersionCmd -> Cmd.versionCmd
            Opt.IngestCmd ingestOpts -> Cmd.ingestCmd ingestOpts
            Opt.PublishCmd publishOpts -> Cmd.publishCmd publishOpts
            Opt.ProduceCmd produceOpts -> Cmd.produceCmd produceOpts
            Opt.WorkCmd workOpts -> Cmd.workCmd workOpts
            Opt.ListCmd listOpts -> Cmd.listCmd listOpts
            -- Opt.LaunchCmd launchOpts -> Cmd.launchCmd launchOpts
      eiRtOptions <- Opt.mergeOptions cliOptions fileOptions envOptions
      case eiRtOptions of
        Left errMsg -> error $ "@[runWithOptions] mergeOptions err: " <> errMsg
        Right rtOptions -> do
          result <- cmdExecutor rtOptions
          -- TODO: return a properly kind of conclusion.
          pure ()
