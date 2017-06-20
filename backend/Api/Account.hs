module Api.Account where

import Servant
import Database.Persist.Sql
import Control.Monad.IO.Class

import Model

type AccountApi = "account" :> Get '[JSON] [Entity Account]

accountServer :: ConnectionPool -> Entity User -> Server AccountApi
accountServer pool user = getListH 
    where
        getListH             = liftIO (getAccounts   pool user)

getAccounts :: ConnectionPool -> Entity User -> IO [Entity Account]
getAccounts pool _ =
    flip runSqlPersistMPool pool $ selectList [] []
    --
    -- let
    --     makeAccountView :: (Account a) => Entity a -> AccountView
    --     makeAccountView (Entity _ x) = toAccountView x
    --     mapAccountView :: (Account a) => [Entity a] -> [AccountView]
    --     mapAccountView = map makeAccountView
    -- in
    --     do
    --         customers <- flip runSqlPersistMPool pool $ selectList [CustomerOwner ==. userKey] []
    --         storages <- flip runSqlPersistMPool pool $ selectList [StorageOwner ==. userKey] []
    --         let customerViews = mapAccountView customers
    --         let storageViews = mapAccountView storages
    --         let userView = toAccountView user
    --         return $ userView:(customerViews ++ storageViews)
