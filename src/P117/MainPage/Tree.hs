{-# LANGUAGE OverloadedStrings #-}

module P117.MainPage.Tree where

import Control.Exception.Lifted
import Control.Monad.Error




import Data.Tree
import Database.HDBC

import Happstack.Server
import P117.DBAccess
import P117.Types





import Text.JSON

{-
treeToHtml :: Integer -> [Tree TreeItem] -> Html
treeToHtml predicateId branches = do
    H.div ! A.id "tree1"
          ! dataAttribute "selectedPath" "-1"
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
-}

treeToJSON :: [Tree TreeItem] -> JSValue
treeToJSON branches = JSArray $ map treeToJSON' branches
    where
    treeToJSON' :: Tree TreeItem -> JSValue
    treeToJSON' (Node item children) =
        makeObj
            [ ("title", JSString $ toJSString $ tiTitle item)
            , ("pageId", JSString $ toJSString $ show $ tiPageId item)
            , ("children", JSArray $ map treeToJSON' children)
            ]

testForest :: Forest TreeItem
testForest = [ Node (TreeItem "MyRoot" 1)
                    [ Node (TreeItem "item1" 1) []
                    , Node (TreeItem "item2" 1)
                           [ Node (TreeItem "item3" 1) []
                           , Node (TreeItem "item4" 1) []
                           , Node (TreeItem "item5" 1) []
                           , Node (TreeItem "item6" 1) []
                           ]
                    ]
             , Node (TreeItem "MyRoot2" 1) []
             , Node (TreeItem "MyRoot3" 1)
                    [Node (TreeItem "item2" 1) []]
             ]

testJSON :: [JSValue]
testJSON = [ makeObj [ ("title", JSString $ toJSString "Item1")
                     , ("pageId", JSString $ toJSString $ show 1)
                     , ("children", JSNull )
                     ]
           , makeObj [ ("title", JSString $ toJSString "Folder 2")
                     , ("pageId", JSString $ toJSString $ show 2)
                     , ("children", JSArray
                           [ makeObj [ ("title", JSString $ toJSString "sub-item 2.1")
                                     , ("pageId", JSString $ toJSString $ show 3)
                                     , ("children", JSNull )
                                     ]
                           ,  makeObj [ ("title", JSString $ toJSString "sub-item 2.2")
                                     , ("pageId", JSString $ toJSString $ show 4)
                                     , ("children", JSNull )
                                     ]
                           ]
                       )
                     ]
           , makeObj [ ("title", JSString $ toJSString "Item3")
                     , ("pageId", JSString $ toJSString $ show 5)
                     , ("children", JSNull )
                     ]
           ]

getTreeForPredicate :: Integer -> ServerPartT (ErrorT String IO) (Forest TreeItem)
getTreeForPredicate predicateId = do
    -- ErrorT is instance of MonadControlIO, but ServerPartT is not
    -- so, lift from ServerPartT
    lift $ bracket (liftIO p117Connect)
                   (liftIO . disconnect)
                   $ \conn -> do
        -- Every root should have -1 as a parent
        -- We need to get id's in the same order always, so use ORDER BY
        r <- liftIO $ quickQuery' conn "SELECT DISTINCT value2 FROM binaryTrue WHERE binaryId == ? AND value1 == -1 ORDER BY value2" [toSql predicateId]
        let convRow [pageIdR] = fromSql pageIdR :: Integer
        let rootPages = map convRow r
        mapM (buildTreeFromRoot conn predicateId) rootPages

    where
        buildTreeFromRoot :: IConnection conn => conn -> Integer -> Integer -> (ErrorT String IO) (Tree TreeItem)
        buildTreeFromRoot conn predicateId rootId = do
            -- 1. Получаем title страницы - корня
            r <- liftIO $ quickQuery' conn "SELECT title FROM pages WHERE id == ?" [toSql rootId]
            let convRow :: [SqlValue] -> String
                convRow [titleR] = fromSql titleR
            let rootTitle = convRow $ head r
            -- 2. Получаем список дочерних страниц
            r <- liftIO $ quickQuery' conn "SELECT value2 FROM binaryTrue where binaryId == ? and value1 == ? ORDER BY value2" [toSql predicateId, toSql rootId]
            let convRow :: [SqlValue] -> Integer
                convRow [idR] = fromSql idR :: Integer
            let childrenId = map convRow r
            -- 3. Строим дочерние деревья
            children <- mapM (buildTreeFromRoot conn predicateId) childrenId
            -- 4. Возвращаем всё дерево
            return $ Node (TreeItem rootTitle rootId) children

getFlatTree :: ServerPartT (ErrorT String IO) (Forest TreeItem)
getFlatTree = do
    lift $ bracket (liftIO p117Connect)
                   (liftIO . disconnect)
                   $ \conn -> do
        r <- liftIO $ quickQuery' conn "SELECT id, title FROM pages ORDER BY title" []
        let convRow :: [SqlValue] -> (Integer, String)
            convRow [v, t] = (fromSql v, fromSql t)
        let rootPages = map convRow r
        let rootPages' = map (\(v,t) -> Node (TreeItem t v) []) rootPages
        return rootPages'
