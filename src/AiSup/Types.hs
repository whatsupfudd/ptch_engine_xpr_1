{-# LANGUAGE DeriveGeneric #-}
module AiSup.Types where

import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import GHC.Generics (Generic)

import qualified Data.Aeson as Ae
import Data.Aeson ((.:))
import qualified Network.HTTP.Client as Hc

data AiClient = AiClient
  { manager :: Hc.Manager
  , baseUrl :: String
  , jwt :: Text
  }

data AiLoginResult
  = AiAuthenticated AiSessionItems
  | AiUnauthorized Text
  | AiError Text
  deriving (Eq, Show)

newtype AiSessionItems = AiSessionItems { jwt :: Text }
  deriving (Eq, Show)

instance Ae.FromJSON AiLoginResult where
  parseJSON = Ae.withObject "AiLoginResult" $ \o -> do
    tag <- o .: "tag"
    case (tag :: Text) of
      "AuthenticatedLR" ->
        AiAuthenticated <$> o .: "contents"
      "UnauthorizedLR" ->
        AiUnauthorized <$> o .: "contents"
      "ErrorLR" ->
        AiError <$> o .: "contents"
      other ->
        fail $ "Unknown login tag: " <> T.unpack other

instance Ae.FromJSON AiSessionItems where
  parseJSON = Ae.withObject "AiSessionItems" $ \o ->
    AiSessionItems <$> o .: "jwt"

data AiInvokeResponse = AiInvokeResponse
  { requestEId :: UUID
  , contextEId :: UUID
  , status :: Text
  , result :: Ae.Value
  }
  deriving (Eq, Show, Generic)

instance Ae.FromJSON AiInvokeResponse where
  parseJSON = Ae.withObject "AiInvokeResponse" $ \o ->
    AiInvokeResponse
      <$> o .: "requestEId"
      <*> o .: "contextEId"
      <*> o .: "status"
      <*> o .: "result"


data AiResponseState
  = AiNotReady
  | AiReady UUID
  | AiAborted Text
  | AiOther Ae.Value
  deriving (Eq, Show)


data AiRunnerCfg = AiRunnerCfg
  { baseUrl :: String
  , username :: Text
  , secret :: Text
  , ttsFunctionEid :: UUID
  , ttsVoice :: Maybe Text
  , imageFunctionEid :: UUID
  , imageModel :: Text
  -- , imagePromptPrefix :: Text
  -- , imagePromptPostfix :: Text
  }
  deriving (Show)
