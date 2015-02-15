
module P117.DBAccess where

import Control.Exception.Lifted
import Control.Monad.Error
import Database.HDBC
import Database.HDBC.Sqlite3
import Safe

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

getMinimumIdPredicate :: ErrorT String IO Integer
getMinimumIdPredicate = do
    bracket (liftIO p117Connect)
            (liftIO . disconnect)
            $ \ conn -> do
        r <- liftIO $ quickQuery' conn "SELECT min(id) FROM binary" []
        let predicateIdM = headMay r >>= headMay
        maybe (throwError "Predicate table is empty") (return . fromSql) predicateIdM

verifyPredicateId :: Integer -> ErrorT String IO Integer
verifyPredicateId predicateId = do
    bracket (liftIO p117Connect)
            (liftIO . disconnect)
            $ \ conn -> do
        r <- liftIO $ quickQuery' conn "SELECT count(*) FROM binary where id == ?" [toSql predicateId]
        let countM = headMay r >>= headMay
        count <- maybe (throwError "error 4") (return . fromSql) countM
        if ((count :: Integer) < 1)
            then throwError $ "Predicate " ++ show predicateId ++ " doesn't exist"
            else return predicateId
