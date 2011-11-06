{-# LANGUAGE OverloadedStrings #-}

module P117.MainPage.Tree where

import Control.Exception.Control
import Control.Monad.Error
import Control.Monad.State.Lazy
import Data.Maybe
import Data.Monoid
import Data.String
import Data.Tree
import Database.HDBC
import Database.HDBC.Sqlite3
import Happstack.Server
import P117.DBAccess
import P117.Types
import Safe
import Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

treeToHtml :: Integer -> [Tree TreeItem] -> Html
treeToHtml predicateId branches = do
    H.div ! A.id "mainTree"
          ! dataAttribute "selectedItemId" "-1"
          ! dataAttribute "predicateId" (fromString $ show predicateId)
          ! A.onclick "tree_toggle(arguments[0])"
          $ do
        H.div $ "Tree"
        H.ul ! A.class_ "Container" $ do
            let initBranch = initSafe branches
            let lastBranchM = lastMay branches
            mapM_ (treeToHtmlRoot' False) initBranch
            maybe mempty (treeToHtmlRoot' True) lastBranchM
    where
        treeToHtmlRoot' :: Bool -> Tree TreeItem -> Html
        treeToHtmlRoot' isLast (Node (TreeItem title pageId itemId) []) = do
            let class_ = if isLast then "Node IsRoot ExpandLeaf IsLast" else "Node IsRoot ExpandLeaf"
            H.li ! A.class_ (fromString class_) $ do
                H.div ! A.class_ "Expand" $ ""
                H.div ! A.class_ "Content"
                      ! dataAttribute "pageId" (fromString $ show pageId)
                      ! dataAttribute "itemId" (fromString $ show itemId)
                    $ H.span ! A.class_ "ItemText"
                             $ fromString title
        treeToHtmlRoot' isLast (Node (TreeItem title pageId itemId) children) = do
            let class_ = if isLast then "Node IsRoot ExpandOpen IsLast" else "Node IsRoot ExpandOpen"
            H.li ! A.class_ (fromString class_) $ do
                H.div ! A.class_ "Expand" $ ""
                H.div ! A.class_ "Content"
                      ! dataAttribute "pageId" (fromString $ show pageId)
                      ! dataAttribute "itemId" (fromString $ show itemId)
                    $ H.span ! A.class_ "ItemText"
                             $ fromString title
                H.ul ! A.class_ "Container" $ do
                    let initChildren = initSafe  children
                    let lastChildM = lastMay children
                    mapM_ (treeToHtml' False) initChildren
                    maybe mempty (treeToHtml' True) lastChildM

        treeToHtml' :: Bool -> Tree TreeItem -> Html
        treeToHtml' isLast (Node (TreeItem title pageId itemId) []) = do
            let class_ = if isLast then "Node ExpandLeaf IsLast" else "Node ExpandLeaf"
            H.li ! A.class_ (fromString class_) $ do
                H.div ! A.class_ "Expand" $ ""
                H.div ! A.class_ "Content"
                      ! dataAttribute "pageId" (fromString $ show pageId)
                      ! dataAttribute "itemId" (fromString $ show itemId)
                    $ H.span ! A.class_ "ItemText"
                             $ fromString title
        treeToHtml' isLast (Node (TreeItem title pageId itemId) children) = do
            let class_ = if isLast then "Node ExpandOpen IsLast" else "Node ExpandOpen"
            H.li ! A.class_ (fromString class_) $ do
                H.div ! A.class_ "Expand" $ ""
                H.div ! A.class_ "Content"
                      ! dataAttribute "pageId" (fromString $ show pageId)
                      ! dataAttribute "itemId" (fromString $ show itemId)
                    $ H.span ! A.class_ "ItemText"
                             $ fromString title
                H.ul ! A.class_ "Container" $ do
                    let initChildren = initSafe  children
                    let lastChildM = lastMay children
                    mapM_ (treeToHtml' False) initChildren
                    maybe mempty (treeToHtml' True) lastChildM

testForest :: Forest TreeItem
testForest = [ Node (TreeItem "MyRoot" 1 0)
                    [ Node (TreeItem "item1" 1 1) []
                    , Node (TreeItem "item2" 1 2)
                           [ Node (TreeItem "item3" 1 3) []
                           , Node (TreeItem "item4" 1 4) []
                           , Node (TreeItem "item5" 1 5) []
                           , Node (TreeItem "item6" 1 6) []
                           ]
                    ]
             , Node (TreeItem "MyRoot2" 1 7) []
             , Node (TreeItem "MyRoot3" 1 8)
                    [Node (TreeItem "item2" 1 9) []]
             ]

getTreeForPredicate :: Integer -> ServerPartT (ErrorT String IO) (Forest TreeItem)
getTreeForPredicate predicateId = do
    -- ErrorT is instance of MonadControlIO, but ServerPartT is not
    -- so, lift from ServerPartT
    lift $ bracket (liftIO p117Connect)
                   (liftIO . disconnect)
                   $ \conn -> do
        r <- liftIO $ quickQuery' conn "SELECT DISTINCT value2 FROM binaryTrue WHERE binaryId == ? AND value1 == -1" [toSql predicateId]
        let convRow [pageIdR] = fromSql pageIdR :: Integer
        let rootPages = map convRow r
        -- rootPagesM <- mapM (checkRootPage conn predicateId) maybeRootPages
        -- let rootPages = catMaybes rootPagesM
        evalStateT ( mapM (buildTreeFromRoot conn predicateId) rootPages
                   ) 0

    where
        {-
        checkRootPage :: IConnection conn => conn -> Integer -> Integer -> ErrorT String IO (Maybe Integer)
        checkRootPage conn predicateId pageId = do
            r <- liftIO $ quickQuery' conn "SELECT COUNT(*) FROM binaryTrue WHERE binaryId == ? and value2 == ?" [toSql predicateId, toSql pageId]
            let convRow [count] = fromSql count :: Integer
            let count = convRow $ head r
            return $ if count == 0 then Just pageId
                                   else Nothing
                                       -}

        buildTreeFromRoot :: IConnection conn => conn -> Integer -> Integer -> StateT Integer (ErrorT String IO) (Tree TreeItem)
        buildTreeFromRoot conn predicateId rootId = do
            -- 1. Получаем title страницы - корня
            r <- liftIO $ quickQuery' conn "SELECT title FROM pages WHERE id == ?" [toSql rootId]
            let convRow :: [SqlValue] -> String
                convRow [titleR] = fromSql titleR
            let rootTitle = convRow $ head r
            -- 2. Получаем список дочерних страниц
            r <- liftIO $ quickQuery' conn "SELECT value2 FROM binaryTrue where binaryId == ? and value1 == ?" [toSql predicateId, toSql rootId]
            let convRow :: [SqlValue] -> Integer
                convRow [idR] = fromSql idR :: Integer
            let childrenId = map convRow r
            -- 3. Строим дочерние деревья
            children <- mapM (buildTreeFromRoot conn predicateId) childrenId
            -- 4. Возвращаем всё дерево
            itemId <- get
            modify (+1)
            return $ Node (TreeItem rootTitle rootId itemId) children
