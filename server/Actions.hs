{-# LANGUAGE OverloadedStrings #-}
module Actions where
import Model
import Control.Monad.Trans (lift)
import Control.Monad.Reader (ReaderT, ask)
import Data.Text.Lazy (Text)
import Data.Aeson (ToJSON, object, (.=))
import qualified Web.Scotty.Trans as S
import qualified Database.Persist.Postgresql as DB
import Database.Esqueleto

type Action = S.ActionT Text (ReaderT DB.ConnectionPool IO)

-- http://jsonapi.org/

transaction :: DB.SqlPersistT Action a -> Action a
transaction queries = do
	pool <- lift ask
	DB.runSqlPool queries pool

-- Wrap Scotty's json to set the correct content type
jsonApi :: (ToJSON a) => a -> Action ()
jsonApi v = do
	S.setHeader "Content-Type" "application/vnd.api+json; charset=utf-8"
	S.json v

-- Handler errors as per spec
errorHandler :: Text -> Action ()
errorHandler x = do
	jsonApi $ object ["errors" .= object ["title" .= x]]

notFoundHandler :: Action ()
notFoundHandler = errorHandler "Not Found"

actions :: S.ScottyT Text (ReaderT DB.ConnectionPool IO) ()
actions = do
	S.defaultHandler errorHandler
	routes
	S.notFound       notFoundHandler

routes :: S.ScottyT Text (ReaderT DB.ConnectionPool IO) ()
routes = do
	S.get     "/api/test"  testGet
	S.get     "/api/test"  handler
	S.post    "/api/test"  testPost
	S.put     "/api/test"  testGet
	S.delete  "/api/test"  testGet

testGet :: Action ()
testGet = do
	jsonApi $ Dilemma "A" "B"

testPost :: Action ()
testPost = do
	t <- S.jsonData
	jsonApi (t :: Dilemma)

handler :: Action ()
handler = do
	r <- transaction $ do
		dilemmaId <- DB.insert $ Dilemma "Michael" "Dilemma"
		dilemma <- DB.get dilemmaId
		return dilemma
	case r of
		Just n -> jsonApi (n :: Dilemma)
		Nothing -> S.raise "Not found"
