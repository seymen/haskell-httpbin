{-# LANGUAGE OverloadedStrings #-}

module GetHandler where

import qualified Web.Scotty as S
import qualified Data.Text.Lazy as L
import Data.Aeson
import Data.Aeson.Types as AT

newtype Headers = Headers [(L.Text, L.Text)]
                deriving (Show)

instance ToJSON Headers where
  toJSON (Headers hs) = do
    let obj = object $ map lazyTextToJsonPair hs
    object ["headers" .= obj]

lazyTextToJsonPair :: (L.Text, L.Text) -> AT.Pair
lazyTextToJsonPair (a, b) = L.toStrict a .= b

getHandler :: S.ActionM ()
getHandler = do
  headers <- S.headers
  S.json $ Headers headers
