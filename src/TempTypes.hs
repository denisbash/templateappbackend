{-# LANGUAGE DeriveGeneric #-}
module TempTypes (
    Template(..),
    Message(..),
    Status(..),
    readStatus
) where

import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON, FromJSON)

data Status = Done | Editable deriving (Eq, Show, Generic)

data Template = Template{
    templateText :: String,
    templateStatus :: Status} deriving (Eq, Show, Generic)

newtype Message = Message{
    txt :: String
} deriving (Eq, Show, Generic)

instance ToJSON Message

instance ToJSON Status
instance ToJSON Template

instance FromJSON Status
instance FromJSON Template

readStatus :: String -> Maybe Status
readStatus "Done" = Just Done
readStatus "Editable" = Just Editable
readStatus _ = Nothing