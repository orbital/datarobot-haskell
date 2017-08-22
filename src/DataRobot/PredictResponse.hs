{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module DataRobot.PredictResponse
  ( PredictError(..)
  , PredictResult(..)
  , responseResult
  , classProbability
  ) where

import Control.Applicative ((<|>))
import Control.Monad.Catch (Exception)
import Data.Aeson (FromJSON(..), ToJSON, Value)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Safe (headMay)


data PredictError
    = APIError Code Text
    | MissingPrediction
    deriving (Typeable, Show)

instance Exception PredictError


newtype PredictResponse = PredictResponse (Either PredictFailure PredictSuccess )
    deriving (Show, Eq)

instance FromJSON PredictResponse where
    parseJSON v = PredictResponse <$>
      ((Right <$> parseJSON v) <|> (Left <$> parseJSON v))

type Code = Int

data PredictFailure = PredictFailure
  { code :: Code
  , status :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON PredictFailure

data PredictSuccess = PredictSuccess
  { predictions :: [Prediction]
  , execution_time :: Float
  , model_id :: Text
  -- , task :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON PredictSuccess

data Prediction = Prediction
  { prediction :: Value
  , class_probabilities :: Maybe (HashMap Text Float)
  } deriving (Show, Eq, Generic)

instance FromJSON Prediction



-- | Result from the prediction

data PredictResult = PredictResult
  { prediction :: Value
  , predictionTimeMs :: Float
  , modelId :: Text
  , classProbabilities :: Maybe (HashMap Text Float)
  } deriving (Show, Eq, Generic)

instance ToJSON PredictResult

responseResult :: PredictResponse -> Either PredictError PredictResult
responseResult (PredictResponse (Right ps)) =
      fromMaybe (Left MissingPrediction) $ do
        p <- headMay (predictions ps)
        return $ Right $ PredictResult
          { prediction = prediction (p :: Prediction)
          , predictionTimeMs = execution_time ps
          , modelId = model_id ps
          , classProbabilities = class_probabilities p
          }
responseResult (PredictResponse (Left pf)) =
    Left $ APIError (code pf) (status pf)


classProbability :: Text -> PredictResult -> Maybe Float
classProbability c r = do
    cps <- classProbabilities r
    HM.lookup c cps

