{-# LANGUAGE OverloadedStrings #-}

module P117.Utils where

import Happstack.Server.Internal.Types
import Happstack.Server.Response
import Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

htmlWrapper :: Html -> Html
htmlWrapper content = do
    preEscapedString $ "<?xml version=\"1.0\" encoding=\"utf-8\"?>\
 \<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">"
    H.html ! A.xmlns "http://www.w3.org/1999/xhtml" $ do
        H.head $ do
            H.title "117"
            H.meta ! A.name "AUTHOR" ! A.content "Evgeny Tarasov"
            H.meta ! A.httpEquiv "Content-Type" ! A.content "text/html; charset=utf-8"
            H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/static/css/smoothness/jquery-ui-1.8.16.custom.css"
            H.script ! A.type_ "text/javascript" ! A.src "/static/js/jquery-1.6.2.min.js" $ ""
            H.script ! A.type_ "text/javascript" ! A.src "/static/js/jquery-ui-1.8.16.custom.min.js" $ ""
            H.script ! A.type_ "text/javascript" ! A.src "/static/js/p117.js" $ ""
            -- H.link ! A.href "/static/css/p117.css" ! A.rel "stylesheet" ! A.type_ "text/css" ! A.title "p117"
        content

buildResponse :: Html -> Response
buildResponse content =
    let response = toResponse $ htmlWrapper content
    in response {rsHeaders = mkHeaders [("Content-type", "text/html; charset=utf8")]}
