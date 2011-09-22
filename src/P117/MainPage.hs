{-# LANGUAGE OverloadedStrings #-}

module P117.MainPage where

import Control.Monad.Error
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
    return $ buildResponse $ do
        "Проект 117"
        H.div ! A.id "mainTree" $ ""

pageHandlerPost :: ServerPartT (ErrorT String IO) Response
pageHandlerPost = undefined
