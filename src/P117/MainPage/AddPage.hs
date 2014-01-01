{-# LANGUAGE OverloadedStrings #-}

module P117.MainPage.AddPage where

import Control.Exception.Lifted
import Control.Monad.Error
import Data.Maybe
import Data.String
import Data.Tree
import Database.HDBC
import Database.HDBC.Sqlite3
import Happstack.Server
import P117.Utils
import Safe
import Text.Blaze
import Text.JSON
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

pageHandler :: ServerPartT (ErrorT String IO) Response
pageHandler = msum [ methodSP GET pageHandlerGet
                   , methodSP POST pageHandlerPost
                   ]

isPageRootPage :: IConnection conn => conn -> Integer -> Integer -> ErrorT String IO Bool
isPageRootPage conn predicateId pageId = do
    r <- liftIO $ quickQuery' conn "SELECT count(*) FROM binaryTrue WHERE binaryId == ? and value2 == ?" [toSql predicateId, toSql pageId]
    let r' = (headMay r >>= headMay) :: Maybe SqlValue
    r'' <- maybe (throwError "error checking if the page is root page") (return . fromSql) r'
    let _ = r'' :: Integer
    if (r'' == 0) then return True
                  else return False

pageHandlerGet :: ServerPartT (ErrorT String IO) Response
pageHandlerGet = do
    pageId <- getInputRead "pageId"
    predicateId <- getInputRead "predicateId"
    let _ = pageId + predicateId :: Integer
    -- Check if the page is root page
    rootPage <- lift $ bracket (liftIO $ connectSqlite3 "sql/test.db")
                               (liftIO . disconnect)
                               $ \ conn -> isPageRootPage conn predicateId pageId

    let title = "New Page"
    let text = "Please write page text"

    return $ toResponse $ do
        H.button ! A.id "previewButton" $ "Preview"
        H.button ! A.id "submitButton" $ "Submit"
        H.br
        H.form ! A.id "addForm" ! A.method "post" $ do
            "Create new page"
            H.select ! A.name "insertPosition" $ do
                H.option ! A.value "inside" $ "Inside selected page"
                when (not rootPage) $
                    H.option ! A.value "after" $ "After selected page on the same level"
                H.option ! A.value "root" $ "as root page"
            H.br
            H.input ! A.type_ "text" ! A.name "title" ! A.value (fromString title)
            H.br
            H.textarea ! A.name "text" ! A.cols "80" ! A.rows "25" $ fromString text

pageHandlerPost :: ServerPartT (ErrorT String IO) Response
pageHandlerPost = do
    relativePageId <- getInputRead "pageId"
    parentId <- getInputRead "parentId"
    predicateId <- getInputRead "predicateId"
    let _ = relativePageId + predicateId + parentId :: Integer
    insertPosition <- getInputString "insertPosition"
    title <- getInputString "title"
    text <- getInputString "text"

    pageId <- lift $ bracket (liftIO $ connectSqlite3 "sql/test.db")
                             (liftIO . disconnect)
                             $ \ conn -> do
        liftIO $ run conn "INSERT INTO pages (title, text) VALUES (?, ?)" [toSql title, toSql text]
        -- liftIO $ commit conn
        r <- liftIO $ quickQuery' conn "SELECT max(id) FROM pages WHERE title == ? and text == ?" [toSql title, toSql text]
        let r' = headMay r >>= headMay
        pageId <- maybe (throwError "error getting id of new created page") (return . fromSql) r'
        let _ = pageId :: Integer
        case insertPosition of
            "inside" -> do
                liftIO $ run conn "INSERT INTO binaryTrue (binaryId, value1, value2) VALUES (?, ?, ?)" [toSql predicateId, toSql relativePageId, toSql pageId]
            "after" -> do
                isRelativePageRootPage <- isPageRootPage conn predicateId relativePageId
                when isRelativePageRootPage $
                    throwError "trying to create page on the same level as root page"
                liftIO $ run conn "INSERT INTO binaryTrue (binaryId, value1, value2) VALUES (?, ?, ?)" [toSql predicateId, toSql parentId, toSql pageId]
            "root" -> do
                liftIO $ run conn "INSERT INTO binaryTrue (binaryId, value1, value2) VALUES (?, -1, ?)" [toSql predicateId, toSql pageId]
            a -> throwError $ "unknown insert position: " ++ a
        liftIO $ commit conn
        return pageId

    return $ toResponse $ encode ("ok" :: String, pageId)
