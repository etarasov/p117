{-# LANGUAGE OverloadedStrings #-}

module P117.MainPage where

import           Control.Exception.Lifted
import           Control.Monad.Error
import           Data.Monoid
import           Data.String

import           Database.HDBC
import           Database.HDBC.Sqlite3
import           Happstack.Server
import           P117.DBAccess
import qualified P117.MainPage.EditPage as EditPage
import qualified P117.MainPage.AddPage as AddPage
import           P117.MainPage.Tree
import           P117.Utils

import           Text.Blaze
import           Text.Blaze.Html

import           Text.JSON
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
        when (Prelude.null r) $ throwError $ "Page with id " ++ show pageId ++ " was not found"
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
    -- Input: 
    --      PredicateType1
    --      CustomPredicate1
    --      Path1
    --      PredicateType2
    --      CustomPredicate2
    --      Path2
    --
    -- Output:
    --      Main page in html
    --          Constructed select controls for both trees
    --          Predicate ids are written to DOM of tree controls
    --          Paths are written to DOM of tree controls
    --
    --          Tree controls are constructed later at additional requests to server
    --
    -- if no predicate, look for any predicate in db
    -- if no path, return null path in DOM and client will ask for the path later
    --

    predicate1May <- getInputReadMay "CustomPredicate1"

    predicate1Id <- case predicate1May of
            Nothing ->
                lift getMinimumIdPredicate
            Just predicateId -> do
                lift $ verifyPredicateId predicateId

    path1May <- getInputStringMay "Path1"
    let path1Str = maybe "" id path1May


    predicate2May <- getInputReadMay "CustomPredicate2"

    predicate2Id <- case predicate2May of
            Nothing ->
                lift getMinimumIdPredicate
            Just predicateId -> do
                lift $ verifyPredicateId predicateId

    path2May <- getInputStringMay "Path2"
    let path2Str = maybe "" id path2May

    displayMode1Str <- getInputStringMay "DisplayMode1" >>= return . maybe "custom" id
    displayMode2Str <- getInputStringMay "DisplayMode2" >>= return . maybe "custom" id

    predicates <- lift getPredicates

    return $ buildResponse $ do
        let inputRadio n v = H.input ! A.type_ "radio"
                                     ! A.name ("predicateRadio" <> n)
                                     ! A.value v
                                     ! A.class_ ("predicateRadio" <> n)
        let inputRadioChecked n v c =
              if c then (inputRadio n v ! A.checked "checked") else inputRadio n v
        H.div ! A.id "treeBlock1" $ do
            inputRadioChecked "1" "custom" (displayMode1Str == "custom")
            H.select ! A.id "predicateSelect1" ! A.name "predicate" $ do
                mapM_ (predicateOption predicate1Id) predicates
            H.br
            inputRadioChecked "1" "allpages" (displayMode1Str == "allpages")
            "All pages"
            H.div ! A.id "treeContainer1"
                  ! dataAttribute "selectedPath" (fromString path1Str)
                  ! dataAttribute "predicateId" (fromString $ show predicate1Id)
                  $
                H.div ! A.id "tree1" $ ""
        H.div ! A.id "treeBlock2" $ do
            inputRadioChecked "2" "custom" (displayMode2Str == "custom")
            H.select ! A.id "predicateSelect2" ! A.name "predicate2" $ do
                mapM_ (predicateOption predicate2Id) predicates
            H.br
            inputRadioChecked "2" "allpages" (displayMode2Str == "allpages")
            "All pages"
            H.div ! A.id "treeContainer2"
                  ! dataAttribute "selectedPath" (fromString path2Str)
                  ! dataAttribute "predicateId" (fromString $ show predicate2Id)
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
