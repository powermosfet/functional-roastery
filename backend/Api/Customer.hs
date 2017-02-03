{-# LANGUAGE FlexibleInstances          #-}

module Api.Customer where

import Servant
import Database.Persist.Sql
import Control.Monad.IO.Class
import Control.Monad

import Model

type CustomerApi = "customer" :> Get '[JSON] [Entity Customer]
              :<|> "customer" :> Post '[JSON] (Entity Customer)
              :<|> "customer" :> Capture "key" (Key Customer) :> Get '[JSON] (Maybe (Entity Customer))
              :<|> "customer" :> Capture "key" (Key Customer) :> ReqBody '[JSON] Customer :> Put '[JSON] (Maybe (Entity Customer))
              :<|> "customer" :> Capture "key" (Key Customer) :> Delete '[JSON] String

customerServer :: ConnectionPool -> Entity User -> Server CustomerApi
customerServer pool user = getListH :<|> createH :<|> getSingleH :<|> changeH :<|> deleteH
    where
        getListH             = liftIO (getCustomers   pool user)
        createH              = liftIO (postCustomer   pool user)
        getSingleH key       = liftIO (getCustomer    pool user key)
        changeH key customer = liftIO (putCustomer    pool user key customer)
        deleteH key          = liftIO (deleteCustomer pool user key)

getCustomers :: ConnectionPool -> Entity User -> IO [Entity Customer]
getCustomers pool (Entity userKey _) = flip runSqlPersistMPool pool $
    selectList [CustomerOwner ==. userKey] []

postCustomer :: ConnectionPool -> Entity User -> IO (Entity Customer)
postCustomer pool (Entity userKey _) = flip runSqlPersistMPool pool $ do
    let customer = Customer "" "" 0 userKey
    key <- insert customer
    return $ Entity key customer 

getCustomer :: ConnectionPool -> Entity User -> Key Customer -> IO (Maybe (Entity Customer))
getCustomer pool (Entity userKey _) customerKey = flip runSqlPersistMPool pool $ do
    mCustomer <- get customerKey
    if checkOwner mCustomer userKey
        then return $ mCustomer >>= (Just . Entity customerKey)
        else return Nothing

putCustomer :: ConnectionPool -> Entity User -> Key Customer -> Customer -> IO (Maybe (Entity Customer))
putCustomer pool (Entity userKey _) customerKey customer = flip runSqlPersistMPool pool $ do
    mOldCustomer <- get customerKey
    case mOldCustomer of
        Just oldCustomer -> if checkOwner mOldCustomer userKey
            then do
                let newCustomer = customer { customerOwner = userKey, customerPayable = customerPayable oldCustomer }
                replace customerKey newCustomer
                return $ Just $ Entity customerKey newCustomer
            else return Nothing
        _ -> return Nothing

deleteCustomer :: ConnectionPool -> Entity User -> Key Customer -> IO String
deleteCustomer pool (Entity userKey _) customerKey = flip runSqlPersistMPool pool $ do
    mOldCustomer <- get customerKey
    when (checkOwner mOldCustomer userKey) $ delete customerKey 
    return ""
