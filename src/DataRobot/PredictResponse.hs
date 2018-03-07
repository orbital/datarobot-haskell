{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module DataRobot.PredictResponse
  ( PredictError(..)
  , PredictResult(..)
  , PredictionValue(..)
  , parseResponse
  , predictionValue
  ) where
import Control.Monad.Catch (Exception)
import Data.Aeson (FromJSON(..), ToJSON, Value(..), decode, defaultOptions, genericParseJSON, withObject, (.:), eitherDecode)
import Data.Aeson.Types (Options(..), typeMismatch)
import Data.List (find)
import Data.Maybe (fromMaybe, maybe)
import Data.Text (Text)
import Data.String.Conversions (cs)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Safe (headMay)

import Lens.Micro ((^.))
import Network.Wreq (Response, responseBody, responseHeader)
import Data.ByteString.Lazy (ByteString)


underscorePrefixOptions :: Options
underscorePrefixOptions =
  defaultOptions { fieldLabelModifier = dropWhile (== '_') }

type Code = Int

data PredictError
    = APIError Code Text
    | MissingPrediction
    deriving (Typeable, Show)

instance Exception PredictError


-- Datarobot successful response
data ResponseSuccess = ResponseSuccess
  { _data :: [Prediction]
  } deriving (Eq, Show, Generic)

instance FromJSON ResponseSuccess where
  parseJSON = genericParseJSON underscorePrefixOptions


-- Datarobot failure response
data ResponseFailure = ResponseFailure
  { _message :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON ResponseFailure where
  parseJSON = genericParseJSON underscorePrefixOptions


-- A single prediction value
data PredictionValue = PredictionValue
  { label :: Text
  , value :: Float
  } deriving (Eq, Show, Generic)

instance ToJSON PredictionValue
instance FromJSON PredictionValue where
  parseJSON = withObject "prediction_value" $ \o -> do
      value' <- o .: "value"
      label' <- labelText =<< o .: "label"
      return $ PredictionValue label' value'
    where
      -- Always treat the label as text even though the JSON also allows numbers
      -- This makes key-based lookup easier on the API consumer
      labelText (Number n) = pure $ (cs .show) n
      labelText (String s) = pure s
      labelText invalid    = typeMismatch "label" invalid


-- Combination of prediction values and prediction label
data Prediction = Prediction
  { _prediction       :: Value  -- label or float
  , _predictionValues :: Maybe [PredictionValue]
  } deriving (Eq, Show, Generic)

instance FromJSON Prediction where
  parseJSON = genericParseJSON underscorePrefixOptions



-- | Result from the prediction
data PredictResult = PredictResult
  { prediction       :: Value
  , predictionTimeMs :: Float
  , predictionValues :: Maybe [PredictionValue]
  } deriving (Show, Eq, Generic)


-- Create a result for a successful response
responseSuccess :: Float -> ResponseSuccess -> Either PredictError PredictResult
responseSuccess et rs =
    maybe (Left MissingPrediction) Right $ do
        p <- headMay (_data rs)
        pure PredictResult
            { prediction       = _prediction p
            , predictionValues = _predictionValues p
            , predictionTimeMs = et
            }

-- Create a result for a failed response
responseFailure :: Text -> Either PredictError PredictResult
responseFailure e = Left $ APIError 422 e

-- Parse the entire prediction response
-- This is needed because some of the data is delivered in the body and some is delivered via headers
parseResponse :: Response ByteString -> Either PredictError PredictResult
parseResponse r = do
    either (responseFailure . cs) (responseSuccess tm) $ eitherDecode b
    where
      b  = r ^. responseBody
      et = r ^. responseHeader "X-DataRobot-Execution-Time"
      tm = fromMaybe 0.0 $ decode (cs et)

-- Find a prediction value probability from a given label
predictionValue :: Text -> PredictResult -> Maybe Float
predictionValue c r = do
    ps <- predictionValues r
    pd <- find ((== c) . label) ps
    pure $ value pd

