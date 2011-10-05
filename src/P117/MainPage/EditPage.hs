{-# LANGUAGE OverloadedStrings #-}

module P117.MainPage.EditPage where

import Control.Exception.Control
import Control.Monad.Error
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

pageHandlerGet :: ServerPartT (ErrorT String IO) Response
pageHandlerGet = do
    pageId <- getInputRead "pageId"
    (shortName, title, text) <- lift $ bracket (liftIO $ connectSqlite3 "sql/test.db")
                                               (liftIO . disconnect)
                                               $ \ conn -> do
        r <- liftIO $ quickQuery' conn "SELECT shortName, title,text FROM pages WHERE id == ?" [toSql (pageId :: Integer)]
        let convRow :: [SqlValue] -> (String, String, String)
            convRow [shortNameRaw, titleRaw, textRaw] = (fromSql shortNameRaw, fromSql titleRaw, fromSql textRaw)
        when (null r) $ throwError $ "Page with id " ++ show pageId ++ " was not found"
        return $ convRow $ head r

    return $ toResponse $ do
        H.button ! A.id "previewButton" $ "Preview"
        H.button ! A.id "submitButton" $ "Submit"
        H.br
        H.form ! A.id "editForm" ! A.method "post" $ do
            H.br
            H.input ! A.type_ "text" ! A.name "shortName" ! A.value (fromString shortName)
            H.input ! A.type_ "text" ! A.name "title" ! A.value (fromString title)
            H.br
            H.textarea ! A.name "text" ! A.cols "80" ! A.rows "25" $ fromString text

pageHandlerPost :: ServerPartT (ErrorT String IO) Response
pageHandlerPost = do
    pageId <- getInputRead "pageId"
    let _ = pageId :: Integer
    shortName <- getInputString "shortName"
    title <- getInputString "title"
    text <- getInputString "text"
    lift $ bracket (liftIO $ connectSqlite3 "sql/test.db")
                   (liftIO . disconnect)
                   $ \ conn -> do
        liftIO $ run conn "UPDATE pages SET shortName=?, title=?, text=? WHERE id = ?" [toSql shortName, toSql title, toSql text, toSql pageId]
        liftIO $ commit conn
    return $ toResponse $ encode ("ok" :: String)
