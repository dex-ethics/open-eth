{-# LANGUAGE OverloadedStrings #-}
module Main where
import Actions
import JsonError
import Control.Applicative ((<$>))
import System.Environment (getEnv)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Web.Scotty (ActionM, ScottyM, middleware, notFound, defaultHandler, scotty, get, post, put, delete)

router :: ScottyM ()
router = do
	middleware          logStdout
	defaultHandler      jsonErrorHandler
	
	get    "/api/test"  testGet
	post   "/api/test"  testPost
	put    "/api/test"  testGet
	delete "/api/test"  testGet
	
	notFound            jsonNotFoundHandler

main :: IO ()
main = do
	port <- read <$> getEnv "PORT"
	scotty port router
