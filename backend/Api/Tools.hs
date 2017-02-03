module Api.Tools where

-- import Servant
import Database.Persist.Sql
import Control.Monad.IO.Class

runDb :: ConnectionPool -> SqlPersistM a -> IO a
runDb pool action = liftIO $ runSqlPersistMPool action pool 
