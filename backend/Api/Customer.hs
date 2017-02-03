{-# LANGUAGE FlexibleInstances          #-}

module Api.Customer where

import Servant
import Database.Persist.Sql
import Control.Monad.IO.Class
import Control.Monad

import Api.Tools
import Model

type CustomerApi = "customer" :> Get '[JSON] [Entity Customer]
              :<|> "customer" :> ReqBody '[JSON] Customer :> Post '[JSON] (Entity Customer)
              :<|> "customer" :> Capture "key" (Key Customer) :> Get '[JSON] (Maybe (Entity Customer))
              :<|> "customer" :> Capture "key" (Key Customer) :> ReqBody '[JSON] Customer :> Put '[JSON] (Maybe (Entity Customer))
              :<|> "customer" :> Capture "key" (Key Customer) :> Delete '[JSON] String

customerServer :: ConnectionPool -> Entity User -> Server CustomerApi
customerServer pool (Entity userKey _) = getListH :<|> createH :<|> getSingleH :<|> changeH :<|> deleteH
    where
        getListH   = getCustomers   (runDb pool) user             
        createH    = postCustomer   (runDb pool) user
        getSingleH = getCustomer    (runDb pool) user
        changeH    = putCustomer    (runDb pool) user
        deleteH    = deleteCustomer (runDb pool) user

getCustomers :: (SqlPersistM [Entity Customer] -> IO [Entity Customer]) -> Entity User -> IO [Entity Customer]
getCustomers db (Entity userKey _) = 
    db $ selectList [CustomerOwner ==. userKey] []

postCustomer :: (SqlPersistM a -> IO a) -> Entity User -> Customer -> IO (Entity Customer)
postCustomer db (Entity userKey _) postedCustomer = do
    let customer = postedCustomer { customerOwner = userKey, customerPayable = 0 }
    key <- db $ insert customer
    return $ Entity key customer 

getCustomer :: (SqlPersistM (Maybe (Entity Customer)) -> IO (Maybe (Entity Customer))) -> Entity User -> Key Customer -> IO (Maybe (Entity Customer))
getCustomer db (Entity userKey _) customerKey = do
    mCustomer <- db $ get customerKey
    if checkOwner mCustomer userKey
        then return $ mCustomer >>= (Just . Entity customerKey)
        else throwError err400

putCustomer :: (SqlPersistM (Maybe (Entity Customer)) -> IO (Maybe (Entity Customer))) -> Entity User -> Key Customer -> Customer -> IO (Entity Customer)
putCustomer db (Entity userKey _) customerKey customer = do
    mOldCustomer <- db $ get customerKey
    case mOldCustomer of
        Just oldCustomer -> if checkOwner mOldCustomer userKey
            then do
                let newCustomer = customer { customerOwner = userKey, customerPayable = customerPayable oldCustomer }
                db $ replace customerKey newCustomer
                return $ Just $ Entity customerKey newCustomer
            else throwError err403
        _ -> throwError err404

deleteCustomer :: (SqlPersistM (Entity Customer) -> IO (Entity Customer)) -> Entity User -> Key Customer -> IO String
deleteCustomer db (Entity userKey _) customerKey = do
    mOldCustomer <- db $ get customerKey
    when (checkOwner mOldCustomer userKey) $ do
        db $ delete customerKey 
        return ""
    throwError err400
