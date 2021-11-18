{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE RankNTypes #-}

module MDB (    
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



insertTemplates :: (Action IO [Value] -> IO [Value]) -> [Template] -> IO [Value]
insertTemplates dbContext templates = dbContext $ insertMany "test_templates" $ map toMDBType templates

allTemplates :: (Action IO [Document] -> IO [Document]) -> IO [Document]
allTemplates dbContext = dbContext $ rest =<< find (select [] "test_templates")