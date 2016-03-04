{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Actions (actions)
import JsonError (errorHandler, notFoundHandler)
import Model (migrateAll, Dilemma)
import Control.Applicative ((<$>))
import System.Environment (getEnv)
import Data.ByteString.Char8 (pack)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Web.Scotty (ActionM, ScottyM, middleware, notFound, defaultHandler, scotty)
import qualified Database.Persist as DB
import qualified Database.Persist.Postgresql as DB
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger


-- https://www.schoolofhaskell.com/school/advanced-haskell/persistent-in-detail/existing-database

-- liftSqlPersistMPool :: MonadIO m => SqlPersistM a -> Data.Pool.Pool SqlBackend -> m a


main :: IO ()
main = do
	
	-- Read environment variables
	db_url  <- pack <$> getEnv "DATABASE_URL"
	db_pool <- read <$> getEnv "DATABASE_POOL_SIZE"
	port    <- read <$> getEnv "PORT"
	
	-- Connect to the database
	runStdoutLoggingT $ DB.withPostgresqlPool db_url db_pool $ DB.liftSqlPersistMPool $ do
		
		-- Run safe database migrations
		DB.runMigration migrateAll
		
		res :: [DB.Entity Dilemma] <- DB.selectList [] [DB.LimitTo 1]
		liftIO $ print res
	
	-- Run the server
	scotty port $ do
		middleware     logStdout
		defaultHandler errorHandler
		actions
		notFound       notFoundHandler
