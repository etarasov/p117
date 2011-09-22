
module P117.MainPage where

import Control.Monad.Error
import Happstack.Server
import P117.Utils
import Text.Blaze

pageHandler :: ServerPartT (ErrorT String IO) Response
pageHandler = msum [ methodSP GET pageHandlerGet
                   , methodSP POST pageHandlerPost
                   ]

pageHandlerGet :: ServerPartT (ErrorT String IO) Response
pageHandlerGet = do
    return $ buildResponse $ toHtml "qwe"

pageHandlerPost = undefined
