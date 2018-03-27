{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module DataRobot.Features where

import Lens.Micro ((.~))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (MonadThrow)
import Data.Monoid ((<>))
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.String.Conversions (cs)

import DataRobot.Types
import DataRobot.API (parseResponse, endpoint)

import Network.URI (URI(..))
import qualified Network.Wreq as Wreq
import Network.Wreq (defaults, header, checkResponse)
import Network.Wreq.Types (ResponseChecker)



-- GET https://app.datarobot.com/api/v2/projects/5988c39bc808917519a2acbb/models/5988d164c8089128924bd6cf/features
features :: (MonadIO m, MonadThrow m) => Credentials -> ProjectID -> ModelID -> m Features
features c pid mid = do
    let opts = httpOptions c
        url = featuresEndpoint (baseURL c) pid mid
    r <- liftIO $ Wreq.getWith opts url
    f <- parseResponse r
    pure f


featuresEndpoint :: URI -> ProjectID -> ModelID -> String
featuresEndpoint base (ProjectID pid) (ModelID mid) =
    endpoint base ["projects", cs pid, "models", cs mid, "features/"]




httpOptions :: Credentials -> Wreq.Options
httpOptions c =
    defaults
      & header "Authorization" .~ [authorization]
      & header "datarobot-key" .~ [apiKey c]
      & checkResponse .~ Just ignoreStatus
  where
    ignoreStatus :: ResponseChecker
    ignoreStatus _ _ = pure ()

    authorization :: ByteString
    authorization = "Token " <> (apiToken c)
