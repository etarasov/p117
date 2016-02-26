{-# LANGUAGE OverloadedStrings #-}

module P117.MainPage.DeletePage where

import Control.Exception.Lifted
import Control.Monad.Error


import Database.HDBC
import Database.HDBC.Sqlite3
import Happstack.Server
import P117.Utils

import Text.JSON

pageHandler :: ServerPartT (ErrorT String IO) Response
pageHandler = msum [ methodSP POST pageHandlerPost ]

pageHandlerPost :: ServerPartT (ErrorT String IO) Response
pageHandlerPost = do
    pageId <- getInputRead "pageId"
    parentId <- getInputRead "parentId"
    mode <- getInputString "mode"
    lift $ bracket (liftIO $ connectSqlite3 "sql/test.db")
                   (liftIO . disconnect)
                   $ \ conn -> do
        case mode of
          "rm_link" -> do
            liftIO $ run conn "DELETE FROM binaryTrue WHERE value1 = ? AND value2 = ?" $ toSql `fmap` ([parentId, pageId] :: [Int])
            [[cnt1]] <- liftIO $ quickQuery' conn "SELECT COUNT(*) FROM binaryTrue WHERE value2 = ?" [toSql pageId]
            if ((fromSql cnt1 :: Int) == 0) then
              liftIO $ run conn "DELETE FROM binaryTrue WHERE value1 = ?" [toSql pageId]
              else return 0
          "rm_all_entries" -> do
            liftIO $ run conn "DELETE FROM binaryTrue WHERE value1 = ? OR value2 = ?" [toSql pageId, toSql pageId]
            liftIO $ run conn "DELETE FROM pages WHERE id = ?" [toSql pageId]
          _ -> throwError "unknown mode"
        liftIO $ commit conn
    return $ toResponse $ encode ("ok" :: String)
