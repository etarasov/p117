
module P117.DBAccess where

import Control.Exception.Control
import Control.Monad.Error
import Database.HDBC
import Database.HDBC.Sqlite3

p117Connect :: IO Connection
p117Connect = connectSqlite3 "sql/test.db"

getPredicates :: ErrorT String IO [(Integer, String)]
getPredicates = do
    bracket (liftIO p117Connect)
            (liftIO . disconnect)
            $ \ conn -> do
        r <- liftIO $ quickQuery' conn "SELECT id,shortName FROM binary" []
        mapM convRow r

    where
        convRow :: [SqlValue] -> ErrorT String IO (Integer, String)
        convRow [] = throwError "Error while getting predicates list 1"
        convRow [rawId, rawShortName] = return (fromSql rawId, fromSql rawShortName)
        convRow _ = throwError "Error while getting predicates list 2"
