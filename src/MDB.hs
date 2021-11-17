{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE RankNTypes #-}

module MDB (
    mainDB,
    insertTemplates,
    allTemplates,
    fromDBType
) where

import Database.MongoDB ( (=:), insertMany, Value (Doc), Action, Document, Val (val, cast'), Field (label, value) )

import Database.MongoDB.Query (access, master, find, rest, Select (select))
import Database.MongoDB.Connection ( connect, host, close )
import TempTypes (Template(..), Status(..), readStatus)
import qualified Data.Text as T

toMDBType :: Template -> Document
toMDBType (Template txt status) = ["templateText" =: txt, "templateStatus" =: show status]

fromDBType :: Document -> Maybe Template
fromDBType d = if length d /= 3 then Nothing else Template <$> toTemplateText (d !! 1) <*> toTemplateStatus (d !! 2)

toTemplateText :: Field -> Maybe String
toTemplateText f = if label f /= "templateText" then Nothing else T.unpack <$> cast' (value f)

toTemplateStatus :: Field -> Maybe Status
toTemplateStatus f = if label f /= "templateStatus" then Nothing else readStatus . T.unpack =<< cast' (value f)

instance Val Template where
   val = Doc . toMDBType
   cast' (Doc fs) = fromDBType fs
   cast' _ = Nothing



mainDB :: IO ()
mainDB = do
   pipe <- connect (host "127.0.0.1")
   e <- access pipe master "baseball" insertTeams
   close pipe
   print e

insertTeams :: Action IO [Value]
insertTeams = insertMany "team" [
   ["name" =: "Yankees", "home" =: ["city" =: "New York", "state" =: "NY"], "league" =: "American"],
   ["name" =: "Mets", "home" =: ["city" =: "New York", "state" =: "NY"], "league" =: "National"],
   ["name" =: "Phillies", "home" =: ["city" =: "Philadelphia", "state" =: "PA"], "league" =: "National"],
   ["name" =: "Red Sox", "home" =: ["city" =: "Boston", "state" =: "MA"], "league" =: "American"] ]

insertTemplates :: (Action IO [Value] -> IO [Value]) -> [Template] -> IO [Value]
insertTemplates dbContext templates = dbContext $ insertMany "test_templates" $ map toMDBType templates

allTemplates :: (Action IO [Document] -> IO [Document]) -> IO [Document]
allTemplates dbContext = dbContext $ rest =<< find (select [] "test_templates")