{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module DataRobot.Predict
  ( predict
  , predictURI
  , Credentials(..)
  , ProjectID(..)
  , ModelID(..)
  ) where

import Lens.Micro ((?~), (.~))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (MonadThrow)
import Data.Aeson (Value(..), encode)
import Data.Function ((&))
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Data.String.Conversions (cs)

import DataRobot.Types
import DataRobot.API (endpoint)
import DataRobot.PredictResponse

import Network.URI (URI(..))
import qualified Network.Wreq as Wreq
import Network.Wreq (defaults, Auth, basicAuth, auth, header, checkResponse)
import Network.Wreq.Types (ResponseChecker)


-- https://app.datarobot.com/docs/users-guide/basics/predictions/prediction-api.html
predict :: (MonadIO m, MonadThrow m) => Credentials -> ProjectID -> ModelID -> Fields -> m (Either PredictError PredictResult)
predict c pid mid o = do
  let opts = httpOptions c
      url = predictURI (baseURLPredict c) pid mid
      body = Array $ V.singleton $ Object $ HM.fromList o
  r <- liftIO $ Wreq.postWith opts url body
  let pr = parseResponse mid r
  pure $ responseResult pr


httpOptions :: Credentials -> Wreq.Options
httpOptions c =
    defaults
      & auth ?~ (authorization c)
      & header "datarobot-key" .~ [apiKey c]
      & checkResponse .~ Just ignoreStatus
  where
    ignoreStatus :: ResponseChecker
    ignoreStatus _ _ = pure ()



predictURI :: URI -> ProjectID -> ModelID -> String
predictURI base (ProjectID pid) (ModelID mid) =
    endpoint base [cs pid, cs mid, "predict"]



authorization :: Credentials -> Auth
authorization c =
  basicAuth (username c) (apiToken c)


