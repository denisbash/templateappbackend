{-# LANGUAGE DeriveGeneric #-}
module TempTypes (
    NamedTemplate(..),
    Message(..),
    Status(..),
    Template(..),
    readStatus
) where

import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON, FromJSON)

data Status = Done | Editable deriving (Eq, Show, Generic)

data NamedTemplate = NamedTemplate{
    templateName :: String,
    templateStatus :: Status,
    template :: Template} deriving (Eq, Show, Generic)

data Template = V String | L [Template] deriving (Eq, Show, Generic)

newtype Message = Message{
    txt :: String
} deriving (Eq, Show, Generic)

instance ToJSON Message

instance ToJSON Status
instance ToJSON NamedTemplate

instance FromJSON Status
instance FromJSON NamedTemplate

instance ToJSON Template
instance FromJSON Template

readStatus :: String -> Maybe Status
readStatus "Done" = Just Done
readStatus "Editable" = Just Editable
readStatus _ = Nothing