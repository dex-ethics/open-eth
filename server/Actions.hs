{-# LANGUAGE OverloadedStrings #-}
module Actions where
import Model
import Web.Scotty (ScottyM, ActionM, get, post, put, delete, json, jsonData)

actions :: ScottyM ()
actions = do
	get    "/api/test"  testGet
	post   "/api/test"  testPost
	put    "/api/test"  testGet
	delete "/api/test"  testGet

testGet :: ActionM ()
testGet = do
	json $ Dilemma "Hello" "World"

testPost :: ActionM ()
testPost = do
	t <- jsonData
	json (t :: Dilemma)
