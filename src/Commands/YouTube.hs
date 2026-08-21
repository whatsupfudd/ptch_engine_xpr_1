module Commands.YouTube (youtubeCmd) where

import Control.Applicative ((<|>))

import qualified Data.Text as T
import qualified Data.Text.IO as Tio

import Options.Cli
  ( YouTubeOpts (..)
  , YouTubeSubCmd (..)
  , YouTubeAuthorizeOpts (..)
  )
import Options.Runtime (RunOptions (..))

import Publish.YouTube.OAuth
  ( YouTubeAuthResult (..)
  , authorizeYouTube
  , writeRefreshToken
  )
import Publish.YouTube.Types (YouTubeConfig (..))


youtubeCmd :: YouTubeOpts -> RunOptions -> IO ()
youtubeCmd opts rtOpts =
  case opts.commandYoutube of
    AuthorizeYouTubeCmd authOpts ->
      authorizeCmd authOpts rtOpts.youTubeConf


authorizeCmd :: YouTubeAuthorizeOpts -> YouTubeConfig -> IO ()
authorizeCmd opts conf = do
  result <-
    authorizeYouTube
      conf
      (not opts.noBrowser)
      opts.timeoutSeconds
      opts.loginHint

  let
    mbPath =
      opts.pathTokenRefresh <|> conf.pathTokenRefresh

  case mbPath of
    Just path -> do
      writeRefreshToken path result.tokenRefreshResult

      putStrLn ""
      putStrLn "YouTube authorization completed."
      putStrLn $ "Refresh token written to: " <> path
      putStrLn $
        "Granted scopes: "
          <> T.unpack (T.unwords result.scopesGranted)

    Nothing -> do
      putStrLn ""
      putStrLn "YouTube authorization completed."
      putStrLn ""
      putStrLn "No refreshTokenFile is configured."
      putStrLn "Store the following value securely as youtube.refreshToken:"
      putStrLn ""
      Tio.putStrLn result.tokenRefreshResult
      putStrLn ""