{-# LANGUAGE DeriveGeneric #-}

module GetHandler where

import           Data.Aeson
import           Data.Aeson.Types as AT
import qualified Data.Text.Lazy   as L
import           GHC.Generics
import qualified Web.Scotty       as S

data Response = Response {
                            args    :: QueryStrings,
                            headers :: Headers
                         } deriving (Generic)

instance ToJSON Response

newtype Headers = Headers [(L.Text, L.Text)]
  deriving (Show)

instance ToJSON Headers where
  toJSON (Headers hs) = object $ map lazyTextToJsonPair hs

newtype QueryStrings = QueryStrings [(L.Text, L.Text)]
  deriving (Show)

instance ToJSON QueryStrings where
  toJSON (QueryStrings qs) = object $ map lazyTextToJsonPair qs

lazyTextToJsonPair :: (L.Text, L.Text) -> AT.Pair
lazyTextToJsonPair (a, b) = L.toStrict a .= b

getHandler :: S.ActionM ()
getHandler = do
  headers <- S.headers
  params <- S.params
  S.json $ Response { args = QueryStrings params, headers = Headers headers }
