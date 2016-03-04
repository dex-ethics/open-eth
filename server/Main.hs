{-# LANGUAGE OverloadedStrings #-}
module Main where
import JsonError (errorHandler, notFoundHandler)
import Model
import Control.Applicative ((<$>))
import System.Environment (getEnv)
import Data.ByteString.Char8 (pack)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Web.Scotty.Trans (ActionT, ScottyT, middleware, notFound, defaultHandler, scottyT, get, json)
import Database.Persist.Postgresql (SqlPersistT, SqlPersistM, runMigration, withPostgresqlPool, insert)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger
import Database.Persist.Postgresql (runSqlPool)
import Database.Esqueleto (entityVal, select, from)


-- http://www.yesodweb.com/book/persistent
-- https://www.schoolofhaskell.com/school/advanced-haskell/persistent-in-detail/existing-database
-- http://taylor.fausak.me/2014/10/21/building-a-json-rest-api-in-haskell/

import Database.Persist.Sql (ConnectionPool)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Reader (Reader, ReaderT, ask, runReader, runReaderT)
import Control.Monad.Identity (Identity)

--type WithLogging = LoggingT IO ()
--type WithPool = ReaderT ConnectionPool WithLogging
--type WithTransaction = WithPool

-- transaction :: (MonadBaseControl IO m, WithPool m) => SqlPersistT m b -> m b
transaction queries = do
	pool <- ask
	runSqlPool queries pool

-- test :: WithPool ()
test = do
	liftIO $ putStrLn "Hello!"

dumpTable :: (MonadIO m, Functor m) => SqlPersistT m [Dilemma]
dumpTable = map entityVal <$> (select . from $ return)


main :: IO ()
main = do
	
	-- Read environment variables
	db_url  <- pack <$> getEnv "DATABASE_URL"
	db_pool <- read <$> getEnv "DATABASE_POOL_SIZE"
	-- port    <- read <$> getEnv "PORT"
	
	
	-- Connect to the database
	runStdoutLoggingT $ withPostgresqlPool db_url db_pool $ runReaderT $ do
		
		test
		
		-- Run safe database migrations
		transaction $ do
			runMigration migrateAll
		
		transaction $ do
			insert $ Dilemma "Hello" "Dilemma"
		
		-- Dump table dilemmas
		transaction $ do
			dilemmas <- dumpTable
			liftIO $ print dilemmas
