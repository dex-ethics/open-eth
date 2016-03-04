{-# LANGUAGE OverloadedStrings #-}
module Actions where
import Model
import Control.Monad.Trans (lift)
import Control.Monad.Reader (ReaderT, ask)
import Data.Text.Lazy (Text)
import Web.Scotty.Trans (ScottyT, ActionT, raise, json, jsonData)
import qualified Web.Scotty.Trans as S
import qualified Database.Persist.Postgresql as DB
import Database.Esqueleto

type Action = S.ActionT Text (ReaderT DB.ConnectionPool IO)

actions :: S.ScottyT Text (ReaderT DB.ConnectionPool IO) ()
actions = do
	S.get    "/api/test"  testGet
	S.get    "/api/test2" handler
	S.post   "/api/test"  testPost
	S.put    "/api/test"  testGet
	S.delete "/api/test"  testGet

transaction :: DB.SqlPersistT Action a -> Action a
transaction queries = do
	pool <- lift ask
	DB.runSqlPool queries pool


testGet :: Action ()
testGet = do
	json $ Dilemma "A" "B"

testPost :: Action ()
testPost = do
	t <- jsonData
	json (t :: Dilemma)

handler :: Action ()
handler = do
	r <- transaction $ do
		dilemmaId <- DB.insert $ Dilemma "Michael" "Dilemma"
		dilemma <- DB.get dilemmaId
		return dilemma
	case r of
		Just n -> json (n :: Dilemma)
		Nothing -> raise "Not found"
