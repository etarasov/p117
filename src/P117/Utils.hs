{-# LANGUAGE OverloadedStrings #-}

module P117.Utils where

import Control.Monad.Error
import Happstack.Server
import Happstack.Server.Internal.Types
import Happstack.Server.Response
import Safe
import Text.Blaze
import Text.Blaze.Html
import Text.Blaze.Internal
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

htmlWrapper :: Html -> Html
htmlWrapper content = do
    preEscapedString $ "<?xml version=\"1.0\" encoding=\"utf-8\"?>\
 \<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">"
    H.html ! A.xmlns "http://www.w3.org/1999/xhtml" $ do
        H.head $ do
            H.title "117"
            H.meta ! A.httpEquiv "Content-Type" ! A.content "text/html; charset=utf-8"
            H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/static/css/smoothness/jquery-ui-1.8.16.custom.css"
            H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/static/css/grid.css"
            H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/static/css/p117.css"
            --H.script ! A.type_ "text/javascript" ! A.src "/static/js/jquery-1.6.2.min.js" $ ""
            H.script ! A.type_ "text/javascript" ! A.src "/static/js/jquery-1.11.0.js" $ ""
            H.script ! A.type_ "text/javascript" ! A.src "/static/js/jquery-ui-1.8.16.custom.min.js" $ ""
            H.script ! A.type_ "text/javascript" ! A.src "/static/js/p117.js" $ ""
            -- H.link ! A.href "/static/css/p117.css" ! A.rel "stylesheet" ! A.type_ "text/css" ! A.title "p117"
            H.script ! A.type_ "text/javascript" ! A.src "/static/js/jquery.dynatree.js" $ ""
            H.link ! A.rel "stylesheet" ! A.href "/static/css/skin/ui.dynatree.css" ! A.type_ "text/css"
            H.script ! A.type_ "text/javascript" ! A.src "/static/js/jquery.cookie.js" $ ""
        content

buildResponse :: Html -> Response
buildResponse content =
    let response = toResponse $ htmlWrapper content
    in response {rsHeaders = mkHeaders [("Content-type", "text/html; charset=utf8")]}

getInputString :: MonadIO m => String -> ServerPartT (ErrorT String m) String
getInputString input = do
    resE <- getDataFn $ look input
    case resE of
        Left _ -> throwError $ "Parameter not found: " ++ input
        Right res -> return res

getInputStringMay :: MonadIO m => String -> ServerPartT (ErrorT String m) (Maybe String)
getInputStringMay input = do
    b <- isThereInput input
    if not b
        then return Nothing
        else getInputString input >>= (return . Just)

getInputNonEmptyString :: MonadIO m => String -> ServerPartT (ErrorT String m) String
getInputNonEmptyString input = do
    res <- getInputString input
    if res == ""
        then throwError $ "Parameter should not be empty: " ++ input
        else return res

getInputOrEmptyString :: MonadIO m => String -> ServerPartT (ErrorT String m) String
getInputOrEmptyString input = do
    b <- isThereInput input
    if not b
        then
            return ""
        else
            getInputString input

getInputRead :: (Read a, MonadIO m) => String -> ServerPartT (ErrorT String m) a
getInputRead input = do
    resS <- getInputString input
    let resM = readMay resS
    case resM of
        Just res -> return res
        Nothing -> throwError $ "Error while parsing parameter: " ++ input

getInputReadMay :: (Read a, MonadIO m) => String -> ServerPartT (ErrorT String m) (Maybe a)
getInputReadMay input = do
    b <- isThereInput input
    if not b
        then return Nothing
        else do
            resS <- getInputString input
            let resM = readMay resS
            maybe (throwError $ "Error while parsing parameter: " ++ input
                              ++ "\nValue: " ++ resS)
                  (return . Just)
                  resM

isThereInput :: MonadIO m => String -> ServerPartT (ErrorT String m) Bool
isThereInput input = do
    resE <- getDataFn $ look input
    return $ case resE of
                Left _ -> False
                Right _ -> True
