
module P117.Utils where

import Happstack.Server

buildResponse :: String -> Response
buildResponse input = let respPlain = toResponse input
                      in respPlain {rsHeaders = mkHeaders [("Content-type", "text/html; charset=utf8")]}

