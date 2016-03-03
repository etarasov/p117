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

structuredErr :: String -> String
structuredErr e = encode e

getParents :: IConnection conn => conn -> Int -> Int -> (ErrorT String IO) [Int]
getParents conn predicateId pageId = do
    r <- liftIO $ quickQuery' conn "SELECT value1 FROM binaryTrue WHERE binaryId = ? AND value2 = ?"
              $ toSql `fmap` ([predicateId,pageId])
    let ps = (\[x] -> fromSql x) `fmap` r
    pps <- getParents conn predicateId `mapM` ps
    return $ ps ++ concat pps
              

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
                   $ toSql `fmap` ([predicateId,targetPageId,srcPageId] :: [Int])
        when ((fromSql cnt :: Int) > 0) $ throwError $ structuredErr "This page already exists with the same parent and predicate"
        parents1 <- getParents conn predicateId targetPageId
        when (srcPageId `elem` parents1) $ throwError $ structuredErr "Can't copy page under itself"
        -- Нельзя копировать страницу внутрь самой себя
        when (srcPageId == targetPageId) $ throwError $ structuredErr "Can't copy page inside itself"
        -- Нельзя копировать страницу внутрь собственных потомков
        [[cnt2]] <- liftIO $ quickQuery' conn "SELECT COUNT(*) FROM binaryTrue WHERE binaryId = ? AND value1 = ? AND value2 = ?" $ toSql `fmap` [predicateId, srcPageId, targetPageId]
        when ((fromSql cnt2 :: Int) > 0) $ throwError $ structuredErr "This parent already a child of this page"
        r <- liftIO $ run conn "INSERT INTO binaryTrue (binaryId, value1, value2) VALUES (?, ?, ?)" $ toSql `fmap` [predicateId, targetPageId, srcPageId]
        liftIO $ commit conn
        return r

    return $ toResponse $ encode ("ok" :: String, pageId)
