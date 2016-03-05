module Main where
import Model (migrateAll)
import Actions (actions)
import Control.Applicative ((<$>))
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Trans (lift)
import Control.Monad.Reader (runReaderT)
import Data.ByteString.Char8 (pack)
import System.Environment (getEnv)
import Database.Persist.Postgresql (withPostgresqlPool, runMigration)
import Database.Persist.Postgresql (runSqlPool)
import Network.Wai.Handler.Warp (Port)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Web.Scotty.Trans (scottyT, middleware)

main :: IO ()
main = do
	
	-- Read environment variables
	db_url  <- pack <$> getEnv "DATABASE_URL"
	db_pool <- read <$> getEnv "DATABASE_POOL_SIZE"
	port    <- read <$> getEnv "PORT"
	
	-- Connect to the database
	runStdoutLoggingT $ withPostgresqlPool db_url db_pool $ \pool -> lift $ do
		
		-- Migrate the database (only does safe migrations)
		flip runSqlPool pool $ do
			runMigration migrateAll
		
		-- Start scotty
		scottyT port (flip runReaderT pool) $ do
			middleware     logStdout
			actions
