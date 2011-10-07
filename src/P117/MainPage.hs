{-# LANGUAGE OverloadedStrings #-}

module P117.MainPage where

import Control.Exception.Control
import Control.Monad.Error
import Data.String
import Data.Tree
import Database.HDBC
import Database.HDBC.Sqlite3
import Happstack.Server
import qualified P117.MainPage.EditPage as EditPage
import qualified P117.MainPage.AddPage as AddPage
import P117.MainPage.Tree
import P117.Utils
import Safe
import Text.Blaze
import Text.Blaze.Internal
import Text.JSON
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

pageHandler :: ServerPartT (ErrorT String IO) Response
pageHandler = msum [ dir "page" (methodSP GET getPage)
                   , dir "tree" (methodSP GET getTree)
                   , dir "editpage" EditPage.pageHandler
                   , dir "addpage" AddPage.pageHandler
                   , methodSP GET pageHandlerGet
                   , methodSP POST pageHandlerPost
                   ]

getPage :: ServerPartT (ErrorT String IO) Response
getPage = do
    pageId <- getInputRead "pageId"
    (title, text) <- lift $ bracket (liftIO $ connectSqlite3 "sql/test.db")
                                    (liftIO . disconnect)
                                    $ \conn -> do
        r <- liftIO $ quickQuery' conn "SELECT title,text FROM pages WHERE id == ?" [toSql (pageId :: Integer)]
        let convRow :: [SqlValue] -> (String, String)
            convRow [titleRaw, textRaw] = (fromSql titleRaw, fromSql textRaw)
        when (null r) $ throwError $ "Page with id " ++ show pageId ++ " was not found"
        return $ convRow $ head r

    return $ toResponse $ do
        H.div ! A.id "buttonBar" $ do
            H.button ! A.id "editButton" $ "Edit"
            H.button ! A.id "addButton" $ "Add"
        H.h1 $ fromString title
        fromString text
        return ()

getTree :: ServerPartT (ErrorT String IO) Response
getTree = do
    predicateId <- getInputRead "predicateId"
    predicateTree <- getTreeForPredicate predicateId

    return $ toResponse $
        treeToHtml predicateId predicateTree

pageHandlerGet :: ServerPartT (ErrorT String IO) Response
pageHandlerGet = do
    let predicateId = 1
    predicateTree <- getTreeForPredicate predicateId

    return $ buildResponse $ do
        H.div ! A.id "treeBlock" $
            treeToHtml predicateId predicateTree
        H.div ! A.id "pageText" $ ""
        H.button ! A.id "testButton" $ "Test"

pageHandlerPost :: ServerPartT (ErrorT String IO) Response
pageHandlerPost = undefined
