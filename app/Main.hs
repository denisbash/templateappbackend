{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Main where

import Lib (app1, appTemplate, appTemplateMDB)
import Network.Wai.Handler.Warp (run)
import Database.MongoDB.Connection ( connect, host, close )
import Database.MongoDB.Query (access, master)

main :: IO ()
main = mainMDB--run 8081 appTemplate--app1

mainMDB :: IO ()
mainMDB = do
    pipe <- connect (host "127.0.0.1")
    let dbContext = access pipe master "templates"
    run 8081 $ appTemplateMDB dbContext
    close pipe
    

