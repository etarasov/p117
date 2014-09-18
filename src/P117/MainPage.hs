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
    tree <- getTreeForPredicate predicateId
    return $ toResponse $ encode $ treeToJSON tree

-- | Get predicates list for rendering in select control with js
getBinaryPredicates :: ServerPartT (ErrorT String IO) Response
getBinaryPredicates = undefined

pageHandlerGet :: ServerPartT (ErrorT String IO) Response
pageHandlerGet = do
    -- Input: predicateId, path
    -- Output: predicateId in DOM + select control with predicates
    --         path in DOM
    -- if no predicate, look for random predicate in db
    -- if on path, return null path in DOM and client will ask for the path later

    predicateMay <- getInputReadMay "predicateId"

    predicateId <- lift $ bracket (liftIO $ connectSqlite3 "sql/test.db")
                                  (liftIO . disconnect)
                                  $ \ conn -> do
        let getMinimumIdPredicate :: IConnection conn => conn -> ErrorT String IO Integer
            getMinimumIdPredicate conn = do
                r <- liftIO $ quickQuery' conn "SELECT min(id) FROM binary" []
                let predicateIdM = headMay r >>= headMay
                maybe (throwError "Predicate table is empty") (return . fromSql) predicateIdM

        case predicateMay of
            Nothing ->
                getMinimumIdPredicate conn
            Just predicateId -> do
                let _ = predicateId :: Integer
                -- Check that predicate exist
                r <- liftIO $ quickQuery' conn "SELECT count(*) FROM binary where id == ?" [toSql predicateId]
                let countM = headMay r >>= headMay
                count <- maybe (throwError "error 4") (return . fromSql) countM
                if ((count :: Integer) < 1)
                    then throwError $ "Predicate " ++ show predicateId ++ " doesn't exist"
                    else return predicateId

    pathMay <- getInputStringMay "Path"
    let pathStr = maybe "" id pathMay

    predicates <- lift getPredicates

    return $ buildResponse $ do
        H.div ! A.id "treeBlock" $ do
            H.input ! A.type_ "radio"
                    ! A.name "predicateRadio1"
                    ! A.value "custom"
                    ! A.class_ "predicateRadio1"
            H.select ! A.id "predicateSelect" ! A.name "predicate" $ do
                mapM_ (predicateOption predicateId) predicates
            H.br
            H.input ! A.type_ "radio"
                    ! A.name "predicateRadio1"
                    ! A.value "allpages"
                    ! A.class_ "predicateRadio1"
            "All pages"
            H.div ! A.id "treeContainer"
                  ! dataAttribute "selectedPath" (fromString pathStr)
                  ! dataAttribute "predicateId" (fromString $ show predicateId)
                  $
                H.div ! A.id "mainTree" $ ""
        H.div ! A.id "treeBlock2" $ do
            H.input ! A.type_ "radio"
                    ! A.name "predicateRadio2"
                    ! A.value "custom"
                    ! A.class_ "predicateRadio2"
            H.select ! A.id "predicateSelect2" ! A.name "predicate2" $ do
                mapM_ (predicateOption predicateId) predicates
            H.br
            H.input ! A.type_ "radio"
                    ! A.name "predicateRadio2"
                    ! A.value "allpages"
                    ! A.class_ "predicateRadio2"
            "All pages"
            H.div ! A.id "treeContainer2"
                  ! dataAttribute "selectedPath" (fromString pathStr)
                  ! dataAttribute "predicateId" (fromString $ show predicateId)
                  $
                H.div ! A.id "tree2" $ ""
        H.div ! A.id "pageText" $ ""

        where
        predicateOption :: Integer -> (Integer, String) -> Html
        predicateOption selectedId (pId, pName) =
            let res = H.option ! A.value (fromString $ show pId) $ fromString pName
            in
            if selectedId == pId
            then
                res ! A.selected "true"
            else
                res

pageHandlerPost :: ServerPartT (ErrorT String IO) Response
pageHandlerPost = undefined
