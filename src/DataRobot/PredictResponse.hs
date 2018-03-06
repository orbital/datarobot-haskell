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
import Data.Aeson (FromJSON(..), ToJSON, Value(..), decode, defaultOptions, genericToJSON, genericParseJSON, withObject, (.:), decodeStrict, eitherDecode)
import Data.Aeson.Types (Options(..))
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Data.String.Conversions (cs)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Safe (headMay)

import Lens.Micro ((^.))
import DataRobot.Types (ModelID(..))
import Network.Wreq (Response, responseBody, responseHeader)
import Data.ByteString.Lazy (ByteString)


underscorePrefixOptions :: Options
underscorePrefixOptions =
  defaultOptions { fieldLabelModifier = dropWhile (== '_') }


data PredictError
    = APIError Int Text
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
  parseJSON = withObject "prediction_value" $ \o ->
    do
      value' <- o .: "value"
      label' <- o .: "label"
      case labelText label' of
        Just l  -> return $ PredictionValue l value'
        Nothing -> fail $ "Invalid label: " <> show label'
    where
      -- Always treat the label as text even though the JSON allows for numbers
      -- This makes label based lookup easier on the API consumer
      labelText (Number n) = Just $ (pack . show) n
      labelText (String s) = Just s
      labelText _ = Nothing

-- Combination of prediction values and prediction label
data Prediction = Prediction
  { prediction       :: Value  -- label or float
  , predictionValues :: Maybe [PredictionValue]
  } deriving (Eq, Show, Generic)

instance FromJSON Prediction


-- | Result from the prediction
data PredictResult = PredictResult
  { prediction       :: Value
  , predictionTimeMs :: Float
  , values           :: Maybe [PredictionValue]
  } deriving (Show, Eq, Generic)


responseResult :: Float -> ResponseSuccess -> Either PredictError PredictResult
responseResult time rs =
    fromMaybe (Left MissingPrediction) $ (Right <$> responseSuccess time rs)

responseFailure :: Text -> PredictError
responseFailure e = APIError 422 e

responseSuccess :: Float -> ResponseSuccess -> Maybe PredictResult
responseSuccess t s = do
    p <- headMay (_data s)
    pure PredictResult
        { prediction       = prediction (p :: Prediction)
        , predictionTimeMs = t
        , values           = predictionValues p
        }

-- Parse the entire prediction response
-- This is needed because some of the data is delivered in the body and some is delivered via headers
parseResponse :: Response ByteString -> Either PredictError PredictResult
parseResponse r = do
    case eitherDecode b of
        Left err ->
            Left $ responseFailure (cs err)
        Right predictions ->
            successResponse predictions
    where
      b  = r ^. responseBody
      et = r ^. responseHeader "X-DataRobot-Execution-Time"

      tm :: Float
      tm = fromMaybe 0.0 $ decode (cs et)

      successResponse :: [Prediction] -> Either PredictError PredictResult
      successResponse preds = responseResult tm $ ResponseSuccess { _data = preds }

predictionValue :: Text -> PredictResult -> Maybe Float
predictionValue c r = do
    ps <- values r
    pd <- find ((== c) . label) ps
    pure $ value pd

