{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Main
Description : The OpenEth server
Copyright   : (c) Dex Ethics, 2016
Maintainer  : Remco Bloemen <openeth@2π.com>
Stability   : experimental
Portability : POSIX
-}

-- https://github.com/scotty-web/scotty/wiki
-- http://taylor.fausak.me/2014/10/21/building-a-json-rest-api-in-haskell/

module Main where

import Control.Applicative ((<$>))
import System.Environment (getEnv)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Web.Scotty (middleware, scotty, get, post, put, delete, json, body, html)
import Data.Text.Lazy (pack)

main :: IO ()
main = do
	port <- read <$> getEnv "PORT"
	scotty port $ do
		middleware logStdoutDev
		get  "/api"      $ html "Hi there"
		get  "/api/port" $ html (pack (show port))
		post "/api/echo" $ do
			stuff <- fmap decodeUtf8 body
			html stuff
