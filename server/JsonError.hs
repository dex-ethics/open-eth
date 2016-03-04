{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module JsonError where
import Actions (Action)
import GHC.Generics (Generic)
import Data.Text.Lazy (Text)
import Web.Scotty.Trans (json)
import Data.Aeson (FromJSON, ToJSON)

-- Error is used by JsonError to communicate errors
-- over HTTP. It is never written to the DB.
data Error =
	Error {
		error         :: !Text
	} deriving (Show, Generic)
instance FromJSON Error
instance ToJSON Error

errorHandler :: Text -> Action ()
errorHandler x = json $ Error x

notFoundHandler :: Action ()
notFoundHandler = json $ Error "Not found"
