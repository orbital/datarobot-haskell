module DataRobot
  (
  -- * Types
    Credentials(..)
  , defaultBaseURL
  , defaultBaseURLPredict
  , ProjectID(..)
  , ModelID(..)

  -- * Features API
  , features
  , Features(..)

  -- * Predict API
  , predict
  , Fields
  , PredictError(..)
  , PredictResult(..)
  , responseResult
  , predictionValue



  ) where

import DataRobot.Features
import DataRobot.Predict
import DataRobot.PredictResponse
import DataRobot.Types
import Network.URI (URI, parseURI)


defaultBaseURL :: URI
defaultBaseURL =
    let (Just u) = parseURI "https://app.datarobot.com/api/v2/"
    in u


defaultBaseURLPredict :: URI
defaultBaseURLPredict =
    let (Just u) = parseURI "https://simplefinance.orm.datarobot.com/api/v1/"
    in u
