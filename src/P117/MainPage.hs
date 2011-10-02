{-# LANGUAGE OverloadedStrings #-}

module P117.MainPage where

import Control.Monad.Error
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


    return $ buildResponse $ do
        "117"
        treeToHtml [ Node "MyRoot"
                          [Node "item1" [], Node "item2" [Node "item3" [], Node "item4" [], Node "item5" [], Node "item6" []]]
                   , Node "MyRoot2" []
                   , Node "MyRoot3"
                          [Node "item2" []]
                   ]
        fromString descr
    {-
        "Проект 117"
        H.div ! A.id "mainTree"
              ! A.onclick "tree_toggle(arguments[0])"
              $ do
            H.div $ "Tree"
            H.ul ! A.class_ "Container" $
                H.li ! A.class_ "Node IsRoot IsLast ExpandClosed" $ do
                    H.div ! A.class_ "Expand" $ ""
                    H.div ! A.class_ "Content" $ "Root"
                    H.ul ! A.class_ "Container" $
                        H.li ! A.class_ "Node ExpandLeaf IsLast" $ do
                            H.div ! A.class_ "Expand" $ ""
                            H.div ! A.class_ "Content" $ "Item"
        H.br
        fromString descr
        -}

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
