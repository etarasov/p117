{-# LANGUAGE OverloadedStrings #-}

module P117.MainPage where

import Control.Exception.Lifted
import Control.Monad.Error
import Data.String
import Data.Tree
import Database.HDBC
import Database.HDBC.Sqlite3
import Happstack.Server
import P117.DBAccess
import qualified P117.MainPage.EditPage as EditPage
import qualified P117.MainPage.AddPage as AddPage
import P117.MainPage.Tree
import P117.Utils
import Safe
import Text.Blaze
import Text.Blaze.Html
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
                                    $ \ conn -> do
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
        preEscapedString text
        return ()

getTree :: ServerPartT (ErrorT String IO) Response
getTree = do
    predicateId <- getInputRead "predicateId"
    predicateTree <- getTreeForPredicate predicateId

    return $ toResponse $
        treeToHtml predicateId predicateTree

-- | Get predicates list for rendering in select control with js
getBinaryPredicates :: ServerPartT (ErrorT String IO) Response
getBinaryPredicates = undefined

pageHandlerGet :: ServerPartT (ErrorT String IO) Response
pageHandlerGet = do
    predicateIdE <- getDataFn $ readCookieValue "predicate"

    predicateId <- lift $ bracket (liftIO $ connectSqlite3 "sql/test.db")
                                  (liftIO . disconnect)
                                  $ \ conn -> do
        -- Check that there is at least one predicate in db
        r <- liftIO $ quickQuery' conn "SELECT count(*) FROM binary" []
        let countMay = headMay r >>= headMay
        predicateCount <- maybe (throwError "error 2") (return . fromSql) countMay
        if ((predicateCount :: Integer) < 1)
            then
                throwError "There are no predicates in base!"
            else do
                let getMinimumIdPredicate :: IConnection conn => conn -> ErrorT String IO Integer
                    getMinimumIdPredicate conn = do
                        r <- liftIO $ quickQuery' conn "SELECT min(id) FROM binary" []
                        let predicateIdM = headMay r >>= headMay
                        maybe (throwError "error 3") (return . fromSql) predicateIdM

                -- Check a cookie
                case predicateIdE of
                    Left _ ->
                        -- Get a predicate with minimum id from base
                        getMinimumIdPredicate conn

                    Right predicateId -> do
                        let _ = predicateId :: Integer
                        -- Check that predicate exist
                        r <- liftIO $ quickQuery' conn "SELECT count(*) FROM binary where id == ?" [toSql predicateId]
                        let countM = headMay r >>= headMay
                        count <- maybe (throwError "error 4") (return . fromSql) countM
                        if ((count :: Integer) < 1)
                            then getMinimumIdPredicate conn
                            else return predicateId

    addCookie (MaxAge $ 60*60*24*365*10) (mkCookie "predicate" $ show predicateId)

    predicateTree <- getTreeForPredicate predicateId

    predicates <- lift getPredicates

    return $ buildResponse $ do
        H.div ! A.id "treeBlock" $ do
            H.select ! A.id "predicateSelect" ! A.name "predicate" $ do
                mapM_ (predicateOption predicateId) predicates
            H.div ! A.id "treeContainer" $
                treeToHtml predicateId predicateTree
        H.div ! A.id "pageText" $ ""

    where
        predicateOption :: Integer -> (Integer, String) -> Html
        predicateOption selectedId (pId, pName) = do
            H.option ! A.value (fromString $ show pId) $ fromString pName

pageHandlerPost :: ServerPartT (ErrorT String IO) Response
pageHandlerPost = undefined
