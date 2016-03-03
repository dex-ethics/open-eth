{-# LANGUAGE OverloadedStrings #-}
module JsonError where
import Model
import Data.Text.Lazy (Text)
import Web.Scotty (ActionM, json)

jsonErrorHandler :: Text -> ActionM ()
jsonErrorHandler x = do
	json $ Error x

jsonNotFoundHandler :: ActionM ()
jsonNotFoundHandler = do
	json $ Error "Not found"
