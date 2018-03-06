{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module DataRobot.PredictResponse
  ( PredictError(..)
  , PredictResult(..)
  , PredictionValue(..)
  , responseResult
  , parseResponse
  , predictionValue
  ) where 
import Control.Monad.Catch (Exception)
import Data.Aeson (FromJSON(..), ToJSON, Value(..), withObject, (.:), decodeStrict, eitherDecode)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Safe (headMay)

import Lens.Micro ((^.))
import DataRobot.Types (ModelID(..))
import Network.Wreq (Response, responseBody, responseHeader)
import Data.ByteString.Lazy (ByteString)


data PredictError
    = APIError Int Text
    | MissingPrediction
    deriving (Typeable, Show)

instance Exception PredictError


data ResponseBody = ResponseBody
  { _predictions :: [Prediction]
  } deriving (Eq, Show, Generic)

instance FromJSON ResponseBody where
  parseJSON = withObject "response_body" $ \o -> do
    d <- o .: "data"   -- predictions
    pure $ ResponseBody { _predictions = d }


data PredictSuccess = PredictSuccess
  { predictions    :: [Prediction]
  , model_id       :: Text
  , execution_time :: Float
  } deriving (Show, Eq, Generic)

data PredictFailure = PredictFailure
  { message :: Text
  } deriving (Show, Eq, Generic)

type PredictResponse
    = Either PredictFailure PredictSuccess


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

data Prediction = Prediction
  { prediction       :: Value
  , predictionValues :: Maybe [PredictionValue]
  } deriving (Eq, Show, Generic)

instance FromJSON Prediction



-- | Result from the prediction

data PredictResult = PredictResult
  { prediction       :: Value
  , predictionTimeMs :: Float
  , modelId          :: Text
  , values           :: Maybe [PredictionValue]
  } deriving (Show, Eq, Generic)


responseResult :: PredictResponse -> Either PredictError PredictResult
responseResult (Right ps) = do
      fromMaybe (Left MissingPrediction) $ do
        p <- headMay (predictions ps)
        pure $ Right $ PredictResult
          { prediction       = prediction (p :: Prediction)
          , predictionTimeMs = execution_time ps
          , modelId          = model_id ps
          , values           = predictionValues p
          }
responseResult (Left pf) = do
    Left $ APIError 422 (message pf)

-- Parse the entire prediction response
-- This is needed because some of the data is delivered in the body and some is delivered via headers
parseResponse :: ModelID -> Response ByteString -> PredictResponse
parseResponse (ModelID mi) r =
    let b  = r ^. responseBody
        et = r ^. responseHeader "X-DataRobot-Execution-Time"
    in
      case eitherDecode b of
        Left err ->
          Left $ PredictFailure { message = pack err }

        Right body ->
          Right $ PredictSuccess
            { predictions    = _predictions body
            , model_id       = mi
            , execution_time = fromMaybe 0.0 (decodeStrict et)
            }

predictionValue :: Text -> PredictResult -> Maybe Float
predictionValue c r = do
    ps <- values r
    pd <- find ((== c) . label) ps
    pure $ value pd

