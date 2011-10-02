{-# LANGUAGE OverloadedStrings #-}

module P117.MainPage where

import Control.Monad.Error
import Data.String
import Database.HDBC
import Database.HDBC.Sqlite3
import Happstack.Server
import P117.Utils
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

pageHandlerPost :: ServerPartT (ErrorT String IO) Response
pageHandlerPost = undefined

data TreeElem = TreeElem { teTitle :: String
                         , teChildren :: [TreeElem]
                         }
