{-# LANGUAGE OverloadedStrings #-}
module Actions where
import Model
import Web.Scotty (ActionM, json, jsonData)

testGet :: ActionM ()
testGet = do
	json $ Dilemma "Hello" "World"

testPost :: ActionM ()
testPost = do
	t <- jsonData
	json (t :: Dilemma)
