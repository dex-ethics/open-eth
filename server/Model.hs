{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
module Model where
import Database.Persist.TH (share, mkPersist, sqlSettings, mkMigrate, persistLowerCase)
import Data.Text (Text)

-- https://github.com/yesodweb/persistent/wiki/Persistent-entity-syntax

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
	
	Dilemma         json
		name         Text
		description  Text
		deriving     Show
	
|]
