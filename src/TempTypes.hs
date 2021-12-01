{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module TempTypes (
    NamedTemplate(..),
    Message(..),
    Status(..),
    Template(..),
    readStatus,
    MTemplate(..),
    ITemplate(..),
    Template'(..),
    toITemplate,
    pruneToTmplId
) where

import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON, FromJSON)
import Data.Functor.Identity (Identity (Identity))

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

data Template' m = T' (ReduceIdentity m String) | L' (ReduceIdentity m [Template' m]) 
deriving instance (Eq (ReduceIdentity m String), Eq (ReduceIdentity m [Template' m])) => Eq (Template' m)
deriving instance (Show (ReduceIdentity m String), Show (ReduceIdentity m [Template' m])) => Show (Template' m)
deriving instance (Generic (ReduceIdentity m String), Generic (ReduceIdentity m [Template' m])) => Generic (Template' m)

instance ToJSON ITemplate
instance ToJSON MTemplate

type MTemplate = Template' Maybe
type ITemplate = Template' Identity

type family ReduceIdentity m a where
    ReduceIdentity Identity a = a
    ReduceIdentity m a = m a

s1 :: MTemplate
s1 = T' (Just "abc")

s2 :: ITemplate
s2 = T' "abc"

toITemplate :: MTemplate -> Maybe ITemplate
toITemplate (T' Nothing) = Nothing
toITemplate (T' (Just s)) = Just $ T' s
toITemplate (L' Nothing) = Nothing
toITemplate (L' (Just ts)) = case traverse toITemplate ts of
    Nothing -> Nothing
    Just xs -> Just (L' xs)

pruneToTmplId :: Template' Maybe -> Maybe (Template' Identity)
pruneToTmplId (T' Nothing) = Nothing
pruneToTmplId (T' (Just s)) = Just $ T' s
pruneToTmplId (L' Nothing) = Nothing
pruneToTmplId (L' (Just ts)) = let
    pruneL [] = []
    pruneL (y:ys) = case toITemplate y of
        Nothing -> pruneL ys 
        Just z -> z : pruneL ys
    in
    case pruneL ts of
        [] -> Nothing
        xs -> Just $ L' xs

            
x = L' $ Just [T' $ Just "abc", T' Nothing] :: Template' Maybe