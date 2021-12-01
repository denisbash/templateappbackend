{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
-----The next two to make the cors part work ----------
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}



module Lib (
        app1,
        appTemplate,
        appTemplateMDB
    ) where

import Servant.API ( JSON, QueryParam, type (:>), Get, Post, type (:<|>) ((:<|>)), ReqBody, Raw )
import Servant.Server ( Server, Application, serve )
import Servant (Proxy (Proxy), serveDirectoryWebApp)
import Data.Aeson.Types (ToJSON, FromJSON)
import GHC.Generics (Generic)
import qualified Data.Text as T
import Servant.Server.Internal.Handler (Handler)
import Servant.API.WebSocket
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad (forever)
import Network.Wai.Middleware.Cors (simpleCors, CorsResourcePolicy (corsMethods, corsRequestHeaders, corsOrigins), cors, simpleCorsResourcePolicy)
import Network.Wai (Middleware)
import Network.WebSockets (Connection(..), sendTextData, withPingThread)
import Control.Concurrent(threadDelay)

import TempTypes ( Message(Message), NamedTemplate )
import MDB (insertTemplates, fromDBType, allTemplates)
import Database.MongoDB (Action)
import Data.Maybe (catMaybes, mapMaybe)


someFunc :: IO ()
someFunc = putStrLn "someFunc"

--type UserAPI = "users" :> QueryParam "sortby" SortBy :> Get '[JSON] [User]

type UserAPI1 = "users" :> Get '[JSON] [User]
    :<|> "isaac" :> Get '[JSON] User
    :<|> "albert" :> Get '[JSON] User

data SortBy = Age | Name

data User = User{
    name :: String,
    age :: Int,
    email :: String
} deriving (Eq, Show, Generic)

instance ToJSON User


type TemplateAPI = "templates" :> Raw
    :<|> "newtemplates" :> ReqBody '[JSON] [NamedTemplate] :> Post '[JSON] Message

type TemplateDBAPI = "templates" :> Get '[JSON] [NamedTemplate]
    :<|> "templates" :> ReqBody '[JSON] [NamedTemplate] :> Post '[JSON] Message
    :<|> "stream" :> WebSocket


isaac :: User
isaac = User "Isaac Newton" 380 "newton@damtp.uk"

albert :: User
albert = User "Albert Einstein" 142 "aeinstein@ias.edu"

users1 :: [User]
users1 = [isaac, albert]

server1 :: Server UserAPI1
server1 = return users1
    :<|> return isaac
    :<|> return albert

userAPI :: Proxy UserAPI1
userAPI = Proxy

app1 :: Application
app1 = serve userAPI server1

templateAPI :: Proxy TemplateAPI
templateAPI = Proxy

templateDBAPI :: Proxy TemplateDBAPI
templateDBAPI = Proxy

serverTemplate :: Server TemplateAPI
serverTemplate = serveDirectoryWebApp "templatesDir"
    :<|> saveTemplates
    where
        saveTemplates :: [NamedTemplate] -> Handler Message
        saveTemplates tmps = do
            liftIO $ appendFile "templatesDir/temps.txt" $ "\n" ++ show tmps
            return $ Message "Oh, yeah"

appTemplate :: Application
appTemplate = corsPolicy $ serve templateAPI serverTemplate

corsPolicy :: Middleware
corsPolicy = cors (const $ Just policy)
    where
        policy = simpleCorsResourcePolicy
          {
              corsMethods = [ "GET", "POST", "PUT", "OPTIONS" ],
              corsOrigins = Just (["http://localhost:3003"], True), -- here is the frontend's URL
              corsRequestHeaders = [ "authorization", "content-type" ]
          }

serverTemplateMDB :: (forall a. Action IO a -> IO a) -> Server TemplateDBAPI
serverTemplateMDB dbContext = getTemplatesDB
    :<|> saveTemplatesDB
    :<|> streamData
    where saveTemplatesDB :: [NamedTemplate] -> Handler Message
          saveTemplatesDB tmps = do
              liftIO $ insertTemplates dbContext tmps
              return $ Message "Oh, yeah MDB"

          getTemplatesDB :: Handler [NamedTemplate]
          getTemplatesDB = do
              docs <- liftIO $ allTemplates dbContext
              return $ mapMaybe fromDBType docs

          streamData :: MonadIO m => Connection -> m ()
          streamData conn = do
              liftIO $ forever $ do
                  docs <- allTemplates dbContext
                  let tmps = mapMaybe fromDBType docs
                  sendTextData conn (T.pack . show $ tmps)
                  threadDelay 30000000              
             
appTemplateMDB :: (forall a. Action IO a -> IO a) -> Application
appTemplateMDB dbContext = corsPolicy $ serve templateDBAPI (serverTemplateMDB dbContext)