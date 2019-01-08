{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module DataRobot.Types where


import Data.ByteString (ByteString)
import Data.Aeson (ToJSON(..), FromJSON(..), Value)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.URI (URI(..))

data Credentials = Credentials
    { apiToken :: ByteString
    , apiKey :: ByteString
    , username :: ByteString
    , baseURLPredict :: URI
    , baseURL :: URI
    } deriving (Show, Eq)


newtype ProjectID = ProjectID Text
  deriving (Show, Eq, Generic, ToJSON, FromJSON)


newtype ModelID = ModelID Text
  deriving (Show, Eq, Generic, ToJSON, FromJSON)


newtype DeploymentID = DeploymentID Text
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data ModelIdentifier = 
    ProjectBase ProjectID ModelID 
    | DeploymentBase DeploymentID 
    deriving (Show, Eq, Generic)
instance ToJSON ModelIdentifier
instance FromJSON ModelIdentifier


newtype Features = Features
    { featureNames :: [Text]
    } deriving (Show, Eq, Generic)
instance ToJSON Features
instance FromJSON Features


type Fields = [(Text, Value)]

