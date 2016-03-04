{-# LANGUAGE OverloadedStrings #-}
module Main where
import JsonError (errorHandler, notFoundHandler)
import Model
import Control.Applicative ((<$>))
import System.Environment (getEnv)
import Data.ByteString.Char8 (pack)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Web.Scotty.Trans (ActionT, ScottyT, middleware, notFound, defaultHandler, scottyT, get, json, raise, liftAndCatchIO)
import Database.Persist.Postgresql (SqlPersistT, SqlPersistM, runMigration, withPostgresqlPool, insert)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger
import Control.Monad.Trans (lift)
import Database.Persist.Postgresql (runSqlPool)
import qualified Database.Persist.Postgresql as DB
import Database.Esqueleto (entityVal, select, from)
import Network.Wai.Handler.Warp (Port)
import Data.Text.Lazy (Text)

-- http://www.yesodweb.com/book/persistent
-- https://www.schoolofhaskell.com/school/advanced-haskell/persistent-in-detail/existing-database
-- http://taylor.fausak.me/2014/10/21/building-a-json-rest-api-in-haskell/

import Database.Persist.Sql (ConnectionPool)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Reader (Reader, ReaderT, ask, runReader, runReaderT)
import Control.Monad.Identity (Identity)
import Network.Wai (Response)

type Action = ActionT Text (ReaderT ConnectionPool IO)

transaction :: SqlPersistT Action a -> Action a
transaction queries = do
	pool <- lift ask
	runSqlPool queries pool

handler :: Action ()
handler = do
	r <- transaction $ do
		dilemmaId <- DB.insert $ Dilemma "Michael" "Dilemma"
		dilemma <- DB.get dilemmaId
		return dilemma
	case r of
		Just n -> json (n :: Dilemma)
		Nothing -> raise "Not found"

main :: IO ()
main = do
	
	-- Read environment variables
	db_url  <- pack <$> getEnv "DATABASE_URL"
	db_pool <- read <$> getEnv "DATABASE_POOL_SIZE"
	port    <- read <$> getEnv "PORT"
	
	-- Connect to the database
	runStdoutLoggingT $ withPostgresqlPool db_url db_pool $ \pool -> liftIO $ do
		
		-- Migrate the database
		flip runSqlPool pool $ do
			runMigration migrateAll
		
		-- Start scotty
		scottyT port (flip runReaderT pool) $ do
			get "/api/test" handler
