{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module JsonError where
import Model
import GHC.Generics (Generic)
import Data.Text.Lazy (Text)
import Web.Scotty (ActionM, json)
import Data.Aeson (FromJSON, ToJSON)

-- Error is used by JsonError to communicate errors
-- over HTTP. It is never written to the DB.
data Error =
	Error {
		error         :: !Text
	} deriving (Show, Generic)
instance FromJSON Error
instance ToJSON Error

errorHandler :: Text -> ActionM ()
errorHandler x = json $ Error x

notFoundHandler :: ActionM ()
notFoundHandler = json $ Error "Not found"
