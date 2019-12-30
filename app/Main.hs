{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty

import GetHandler (getHandler)

main :: IO ()
main = scotty 3000 routes

routes :: ScottyM ()
routes = getRoute

getRoute :: ScottyM ()
getRoute = get "/" getHandler
