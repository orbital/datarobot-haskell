module DataRobot.API where

import Lens.Micro ((^.))
import Data.ByteString.Lazy (ByteString)
import Data.Aeson (FromJSON, eitherDecode)
import Data.List (intercalate)
import Data.Typeable (Typeable)
import Data.Monoid ((<>))
import Control.Monad.Catch (MonadThrow, Exception, throwM)
import Network.URI (URI(..), pathSegments, uriToString)
import Network.Wreq (Response, responseBody)

data JSONError = JSONError String ByteString
               deriving (Typeable, Show)
instance Exception JSONError


parseResponse :: (FromJSON a, MonadThrow m) => Response ByteString -> m a
parseResponse r =
  case eitherDecode $ r ^. responseBody of
    Left err -> throwM $ JSONError err (r ^. responseBody)
    Right p -> pure p

endpoint :: URI -> [String] -> String
endpoint base ps =
    let ps' = "" : pathSegments base <> ps
        u = base { uriPath = intercalate "/" ps' }
    in uriToString id u ""
