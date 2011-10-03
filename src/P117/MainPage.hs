{-# LANGUAGE OverloadedStrings #-}

module P117.MainPage where

import Control.Monad.Error
import Data.String
import Data.Tree
import Database.HDBC
import Database.HDBC.Sqlite3
import Happstack.Server
import P117.MainPage.Tree
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
    predicateTree <- getTreeForPredicate 1

    return $ buildResponse $ do
        "117"
        treeToHtml predicateTree

pageHandlerPost :: ServerPartT (ErrorT String IO) Response
pageHandlerPost = undefined
