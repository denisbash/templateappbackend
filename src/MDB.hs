{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module MDB (
    insertTemplates,
    allTemplates,
    fromDBType
) where

import Database.MongoDB ( (=:), insertMany, Value (Doc, String, Array), Action, Document, Val (val, cast'), Field (label, value) )

import Database.MongoDB.Query (access, master, find, rest, Select (select))
import Database.MongoDB.Connection ( connect, host, close )
import TempTypes (NamedTemplate(..), Status(..), readStatus, Template (..), Template' (V, L))
import qualified Data.Text as T


toMDBType :: NamedTemplate -> Document
toMDBType (NamedTemplate txt status tmp) = ["templateName" =: txt, 
   "templateStatus" =: show status, 
   "template" =: toMDBValue tmp ]

fromDBType :: Document -> Maybe NamedTemplate
fromDBType d = if length d /= 4 
               then Nothing 
               else NamedTemplate 
                 <$> toTemplateText (d !! 1) 
                 <*> toTemplateStatus (d !! 2)
                 <*> toTemplate (d !! 3)

toTemplateText :: Field -> Maybe String
toTemplateText f = if label f /= "templateName" then Nothing else T.unpack <$> cast' (value f)

toTemplateStatus :: Field -> Maybe Status
toTemplateStatus f = if label f /= "templateStatus" then Nothing else readStatus . T.unpack =<< cast' (value f)

toTemplate :: Field -> Maybe Template
toTemplate f = if label f /= "template" then Nothing else cast' (value f)

toMDBValue :: Template -> Value
toMDBValue (V s) = String . T.pack $ s
toMDBValue (L ts) = Array $ map toMDBValue ts

fromMDBValue :: Value -> Maybe Template
fromMDBValue (String vtxt) = Just . V $ T.unpack vtxt
fromMDBValue (Array vs) =  L <$> traverse fromMDBValue vs
fromMDBValue _ = Nothing

instance Val NamedTemplate where
   val = Doc . toMDBType
   cast' (Doc fs) = fromDBType fs
   cast' _ = Nothing

instance Val Template  where
   val = toMDBValue
   cast' = fromMDBValue


insertTemplates :: (Action IO [Value] -> IO [Value]) -> [NamedTemplate] -> IO [Value]
insertTemplates dbContext templates = dbContext $ insertMany "templates" $ map toMDBType templates

allTemplates :: (Action IO [Document] -> IO [Document]) -> IO [Document]
allTemplates dbContext = dbContext $ rest =<< find (select [] "templates")