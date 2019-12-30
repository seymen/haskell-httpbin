{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module GetHandler where

import           Data.Aeson
import           Data.Aeson.Types     as AT
import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as B
import           Data.Maybe
import qualified Data.Text            as T
import           Data.Text.Encoding
import qualified Data.Text.Lazy       as L
import           GHC.Generics
import qualified Network.Wai.Internal as W
import           Web.Scotty           (ActionM, request)
import qualified Web.Scotty           as S

data Response = Response {
                           args    :: QueryStrings
                         , headers :: Headers
                         , url     :: T.Text
                         , origin  :: T.Text
                         } deriving (Generic)

instance ToJSON Response

newtype Headers = Headers [(L.Text, L.Text)]
  deriving (Show)

instance ToJSON Headers where
  toJSON (Headers hs) = object $ map lazyTextToJsonPair hs

newtype QueryStrings = QueryStrings [(ByteString, Maybe ByteString)]
  deriving (Show)

instance ToJSON QueryStrings where
  toJSON (QueryStrings qs) = object $ map queryToJsonPair qs

lazyTextToJsonPair :: (L.Text, L.Text) -> AT.Pair
lazyTextToJsonPair (a, b) = L.toStrict a .= b

queryToJsonPair :: (ByteString, Maybe ByteString) -> AT.Pair
queryToJsonPair (a, m) = x .= y
  where x = decodeUtf8 a
        y = decodeUtf8 $ fromMaybe B.empty m

generateFullUrl :: Bool -> T.Text -> T.Text -> T.Text -> T.Text
generateFullUrl secure host path query = http <> "://" <> host <> path <> query
  where http = if secure then "https" else "http"

getHandler :: ActionM ()
getHandler = do
  req <- request
  headers <- S.headers
  let params = W.queryString req
  let isSecure = W.isSecure req
  let host = decodeUtf8 $ fromMaybe B.empty $ W.requestHeaderHost req
  let path = decodeUtf8 $ W.rawPathInfo req
  let rawQ = decodeUtf8 $ W.rawQueryString req
  S.json $ Response {
                      url = generateFullUrl isSecure host path rawQ
                    , headers = Headers headers
                    , args = QueryStrings params
                    , origin = T.pack $ show $ W.remoteHost req
                    }
