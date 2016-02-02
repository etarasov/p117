{-# LANGUAGE OverloadedStrings #-}

module P117.MainPage.CopyPage where

import Control.Exception.Lifted
import Control.Monad.Error



import Database.HDBC
import Database.HDBC.Sqlite3
import Happstack.Server
import P117.Utils


import Text.JSON



pageHandler :: ServerPartT (ErrorT String IO) Response
pageHandler = msum [ methodSP POST pageHandlerPost
                   ]

jsonErr :: String -> String
jsonErr e = encode ("error" :: String, e)

pageHandlerPost :: ServerPartT (ErrorT String IO) Response
pageHandlerPost = do
    srcPageId <- getInputRead "srcPageId"
    targetPageId <- getInputRead "targetPageId"
    predicateId <- getInputRead "predicateId"
    pageId <- lift $ bracket (liftIO $ connectSqlite3 "sql/test.db")
                             (liftIO . disconnect)
                             $ \ conn -> do
        -- Нельзя копировать страницу к тому же родителю в том же предикате, что и исходная страница, так как такая связь уже есть
        [[cnt]] <- liftIO $ quickQuery' conn "SELECT COUNT(*) FROM binaryTrue WHERE binaryId = ? AND value1 = ? AND value2 = ?"
                   $ toSql <$> ([predicateId,targetPageId,srcPageId] :: [Int])
        when ((fromSql cnt :: Int) > 0) $ throwError $ jsonErr "This page already exists with the same parent and predicate"
        -- Нельзя копировать страницу внутрь самой себя
        when (srcPageId == targetPageId) $ throwError $ jsonErr "Can't copy page inside itself"
        -- Нельзя копировать страницу внутрь собственных потомков
        [[cnt1]] <- liftIO $ quickQuery' conn "SELECT COUNT(*) FROM binaryTrue WHERE binaryId = ? AND value1 = ? AND value2 = ?" $ toSql <$> [predicateId, srcPageId, targetPageId]
        when ((fromSql cnt1 :: Int) > 0) $ throwError $ jsonErr "This parent already a child of this page"
        -- when srcPageId == targetPageId $ throwError $ jsonErr "Can't copy page inside itself"
        r <- liftIO $ run conn "INSERT INTO binaryTrue (binaryId, value1, value2) VALUES (?, ?, ?)" $ toSql <$> [predicateId, targetPageId, srcPageId]
        liftIO $ commit conn
        return r

    return $ toResponse $ encode ("ok" :: String, pageId)
