{-# LANGUAGE OverloadedStrings #-}

module P117.MainPage.AddPredicatePage where

import Control.Exception.Lifted
import Control.Monad.Error
import Data.Char

import Database.HDBC
import Database.HDBC.Sqlite3
import Happstack.Server
import P117.Utils
import Text.JSON


pageHandler :: ServerPartT (ErrorT String IO) Response
pageHandler = msum [ methodSP POST pageHandlerPost ]

validateTitle :: String -> String
validateTitle t = maybe "" id $ validate' t
  where
    validate' x = msum $ fmap (\f -> f x) [vSymbols, vNonEmpty, vLength]
    vLength x = if (length x) <= 40
                then Nothing
                else Just "Length is too big"
    vNonEmpty x = if x /= ""
                  then Nothing
                  else Just "Field is empty"
    vSymbols x = if all (\c -> isAlphaNum c || c == '-' || c == '.') x
                 then Nothing
                 else Just "Field should contain digits, letters, '-' and '.' symbols"
  

pageHandlerPost :: ServerPartT (ErrorT String IO) Response
pageHandlerPost = do
    title <- getInputString "title"

    let err = validateTitle title

    when (err /= "") $ throwError $ encode (err)

    binId <- lift $ bracket (liftIO $ connectSqlite3 "sql/test.db")
                             (liftIO . disconnect)
                             $ \ conn -> do
        liftIO $ run conn "INSERT INTO binary (shortName) VALUES (?)" [toSql title]
        [[binId]] <- liftIO $ quickQuery' conn "SELECT max(id) FROM binary WHERE shortName == ?" [toSql title]
        
        liftIO $ commit conn
        return (fromSql binId :: Int)

    return $ toResponse $ encode ("ok" :: String, binId)
