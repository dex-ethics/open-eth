{-# LANGUAGE OverloadedStrings #-}
module Main where
import Actions
import JsonError
import Control.Applicative ((<$>))
import System.Environment (getEnv)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Web.Scotty (ActionM, ScottyM, middleware, notFound, defaultHandler, scotty, get, post, put, delete)
import qualified Database.HDBC as H
import qualified Database.HDBC.PostgreSQL as H

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
	
	-- Read environment variables
	db_url <- getEnv "DATABASE_URL"
	port   <- read <$> getEnv "PORT"
	
	-- Connect to the database
	conn <- H.connectPostgreSQL db_url
	H.run conn "CREATE TABLE test (id INTEGER NOT NULL)" []
	H.commit conn
	H.disconnect conn
	
	-- Start the server
	scotty port router

