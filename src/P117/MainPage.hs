{-# LANGUAGE OverloadedStrings #-}

module P117.MainPage where

import Control.Exception.Control
import Control.Monad.Error
import Data.Maybe
import Data.Monoid
import Data.String
import Data.Tree
import Database.HDBC
import Database.HDBC.Sqlite3
import Happstack.Server
import P117.Utils
import Safe
import Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

pageHandler :: ServerPartT (ErrorT String IO) Response
pageHandler = msum [ methodSP GET pageHandlerGet
                   , methodSP POST pageHandlerPost
                   ]

pageHandlerGet :: ServerPartT (ErrorT String IO) Response
pageHandlerGet = do

    conn <- liftIO $ connectSqlite3 "sql/test.db"
    r <- liftIO $ quickQuery' conn "SELECT * from pages where id == 1" []
    let convRow [idR, shortNameR, titleR, descrR] = fromSql descrR :: String
    let descr = convRow $ head r
    liftIO $ disconnect conn


    predicateTree <- getTreeForPredicate 1

    return $ buildResponse $ do
        "117"
        treeToHtml predicateTree

pageHandlerPost :: ServerPartT (ErrorT String IO) Response
pageHandlerPost = undefined

treeToHtml :: [Tree String] -> Html
treeToHtml branches = do
    H.div ! A.id "mainTree"
          ! A.onclick "tree_toggle(arguments[0])"
          $ do
        H.div $ "Tree"
        H.ul ! A.class_ "Container" $ do
            let initBranch = initSafe branches
            let lastBranchM = lastMay branches
            mapM_ (treeToHtmlRoot' False) initBranch
            maybe mempty (treeToHtmlRoot' True) lastBranchM
    where
        treeToHtmlRoot' :: Bool -> Tree String -> Html
        treeToHtmlRoot' isLast (Node label []) = do
            let class_ = if isLast then "Node IsRoot ExpandLeaf IsLast" else "Node IsRoot ExpandLeaf"
            H.li ! A.class_ (fromString class_) $ do
                H.div ! A.class_ "Expand" $ ""
                H.div ! A.class_ "Content" $ fromString label
        treeToHtmlRoot' isLast (Node label children) = do
            let class_ = if isLast then "Node IsRoot ExpandClosed IsLast" else "Node IsRoot ExpandClosed"
            H.li ! A.class_ (fromString class_) $ do
                H.div ! A.class_ "Expand" $ ""
                H.div ! A.class_ "Content" $ fromString label
                H.ul ! A.class_ "Container" $ do
                    let initChildren = initSafe  children
                    let lastChildM = lastMay children
                    mapM_ (treeToHtml' False) initChildren
                    maybe mempty (treeToHtml' True) lastChildM

        treeToHtml' :: Bool -> Tree String -> Html
        treeToHtml' isLast (Node label []) = do
            let class_ = if isLast then "Node ExpandLeaf IsLast" else "Node ExpandLeaf"
            H.li ! A.class_ (fromString class_) $ do
                H.div ! A.class_ "Expand" $ ""
                H.div ! A.class_ "Content" $ fromString label
        treeToHtml' isLast (Node label children) = do
            let class_ = if isLast then "Node ExpandClosed IsLast" else "Node ExpandClosed"
            H.li ! A.class_ (fromString class_) $ do
                H.div ! A.class_ "Expand" $ ""
                H.div ! A.class_ "Content" $ fromString label
                H.ul ! A.class_ "Container" $ do
                    let initChildren = initSafe  children
                    let lastChildM = lastMay children
                    mapM_ (treeToHtml' False) initChildren
                    maybe mempty (treeToHtml' True) lastChildM

testForest :: Forest String
testForest = [ Node "MyRoot"
                    [Node "item1" [], Node "item2" [Node "item3" [], Node "item4" [], Node "item5" [], Node "item6" []]]
             , Node "MyRoot2" []
             , Node "MyRoot3"
                    [Node "item2" []]
             ]

getTreeForPredicate :: Integer -> ServerPartT (ErrorT String IO) (Forest String)
getTreeForPredicate predicateId = do
    -- ErrorT is instance of MonadControlIO, but ServerPartT is not
    -- so, lift from ServerPartT
    lift $ bracket (liftIO $ connectSqlite3 "sql/test.db")
                   (liftIO . disconnect)
                   $ \conn -> do
        r <- liftIO $ quickQuery' conn "SELECT DISTINCT value1 FROM binaryTrue WHERE binaryId == ?" [toSql predicateId]
        let convRow [pageIdR] = fromSql pageIdR :: Integer
        let maybeRootPages = map convRow r
        rootPagesM <- mapM (checkRootPage conn predicateId) maybeRootPages
        let rootPages = catMaybes rootPagesM
        mapM (buildTreeFromRoot conn predicateId) rootPages

    where
        checkRootPage :: IConnection conn => conn -> Integer -> Integer -> ErrorT String IO (Maybe Integer)
        checkRootPage conn predicateId pageId = do
            r <- liftIO $ quickQuery' conn "SELECT COUNT(*) FROM binaryTrue WHERE binaryId == ? and value2 == ?" [toSql predicateId, toSql pageId]
            let convRow [count] = fromSql count :: Integer
            let count = convRow $ head r
            return $ if count == 0 then Just pageId
                                   else Nothing

        buildTreeFromRoot :: IConnection conn => conn -> Integer -> Integer -> ErrorT String IO (Tree String)
        buildTreeFromRoot conn predicateId rootId = do
            -- 1. Получаем shortName страницы - корня
            r <- liftIO $ quickQuery' conn "SELECT shortName FROM pages WHERE id == ?" [toSql rootId]
            let rootName = fromSql $ head $ head r
            -- 2. Получаем список дочерних страниц
            r <- liftIO $ quickQuery' conn "SELECT value2 FROM binaryTrue where binaryId == ? and value1 == ?" [toSql predicateId, toSql rootId]
            let convRow [idR] = fromSql idR :: Integer
            let childrenId = map convRow r
            -- 3. Строим дочерние деревья
            children <- mapM (buildTreeFromRoot conn predicateId) childrenId
            -- 4. Возвращаем всё дерево
            return $ Node rootName children
