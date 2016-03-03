{-# LANGUAGE DeriveGeneric #-}
module Model where
import GHC.Generics (Generic)
import Data.Text.Lazy (Text)
import Data.Aeson (FromJSON, ToJSON)

data Error =
	Error {
		error         :: !Text
	} deriving (Show, Generic)
instance FromJSON Error
instance ToJSON Error

data Dilemma =
	Dilemma {
		name          :: !Text,
		description   :: !Text
	} deriving (Show, Generic)
instance FromJSON Dilemma
instance ToJSON Dilemma
