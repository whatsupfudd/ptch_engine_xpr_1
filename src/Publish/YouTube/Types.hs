module Publish.YouTube.Types where

import Data.Text (Text)


data YouTubeConfig = YouTubeConfig
  { idClient :: Text
  , secretClient :: Maybe Text
  , tokenRefresh :: Maybe Text
  , pathTokenRefresh :: Maybe FilePath
  }
  deriving (Eq, Show)


defaultYouTubeConf :: YouTubeConfig
defaultYouTubeConf =
  YouTubeConfig
    { idClient = ""
    , secretClient = Nothing
    , tokenRefresh = Nothing
    , pathTokenRefresh = Nothing
    }


data YouTubeVideoSpec = YouTubeVideoSpec
  { titleVideo :: Text
  , descriptionVideo :: Text
  , tagsVideo :: [Text]
  , idCategory :: Text
  , privacyVideo :: Text
  , notifySubscribers :: Bool
  , madeForKids :: Maybe Bool
  , syntheticMedia :: Maybe Bool
  }
  deriving (Eq, Show)


newtype YouTubeUploadResult = YouTubeUploadResult
  { idVideo :: Text
  }
  deriving (Eq, Show)