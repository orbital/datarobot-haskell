{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module DataRobot.Predict
  ( predict
  --, predictByProjectID
  , predictURI
  , Credentials(..)
  , ProjectID(..)
  , ModelID(..)
  ) where

import Lens.Micro ((?~), (.~))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (MonadThrow)
import Data.Aeson (Value(..))
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
predict :: (MonadIO m, MonadThrow m) => Credentials -> ModelIdentifier -> Fields -> m (Either PredictError PredictResult)
predict c mident o = do
  let opts = httpOptions c
      url = predictURI (baseURLPredict c) mident
      body = Array $ V.singleton $ Object $ HM.fromList o
  r <- liftIO $ Wreq.postWith opts url body
  pure $ parseResponse r

httpOptions :: Credentials -> Wreq.Options
httpOptions c =
    defaults
      & auth ?~ authorization c
      & header "datarobot-key" .~ [apiKey c]
      & checkResponse .~ Just ignoreStatus
  where
    ignoreStatus :: ResponseChecker
    ignoreStatus _ _ = pure ()


predictURI :: URI -> ModelIdentifier -> String
predictURI base (ProjectBase (ProjectID pid) (ModelID mid)) =
    endpoint base [cs pid, cs mid, "predict"]
predictURI base (DeploymentBase (DeploymentID did))= 
    endpoint base ["deployments",cs did, "predictions"]

authorization :: Credentials -> Auth
authorization c =
  basicAuth (username c) (apiToken c)


