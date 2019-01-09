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
    , apiKey   :: ByteString
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

data ModelIdentifier 
  = ProjectBase ProjectID ModelID 
    | DeploymentBase DeploymentID ProjectID ModelID
    deriving (Show, Eq, Generic)
instance ToJSON ModelIdentifier
instance FromJSON ModelIdentifier

projectID :: ModelIdentifier -> ProjectID
projectID (ProjectBase pid _) = pid 
projectID (DeploymentBase _ pid _) = pid 

modelID :: ModelIdentifier -> ModelID
modelID (ProjectBase _ mid) = mid 
modelID (DeploymentBase _ _ mid) = mid 

deploymentID :: ModelIdentifier -> Maybe DeploymentID
deploymentID (ProjectBase _ _) = Nothing 
deploymentID (DeploymentBase did _ _) = Just did  

newtype Features = Features
    { featureNames :: [Text]
    } deriving (Show, Eq, Generic)
instance ToJSON Features
instance FromJSON Features


type Fields = [(Text, Value)]

