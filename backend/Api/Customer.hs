{-# LANGUAGE FlexibleInstances          #-}

module Api.Customer where

import Servant
import Database.Persist.Sql
import Control.Monad.IO.Class

import Model

type CustomerApi = "customer" :> Get '[JSON] [Entity Customer]
              :<|> "customer" :> ReqBody '[JSON] Customer :> Post '[JSON] (Entity Customer)
              :<|> "customer" :> Capture "key" (Key Customer) :> Get '[JSON] (Maybe (Entity Customer))
              :<|> "customer" :> Capture "key" (Key Customer) :> ReqBody '[JSON] Customer :> Put '[JSON] (Maybe (Entity Customer))
              :<|> "customer" :> Capture "key" (Key Customer) :> Delete '[JSON] String

customerServer :: ConnectionPool -> Entity User -> Server CustomerApi
customerServer pool (Entity userKey _) = getCustomers :<|> postCustomer :<|> getCustomer :<|> putCustomer :<|> deleteCustomer
    where
        io pool' action = liftIO $ runSqlPersistMPool action pool'

        getCustomers :: Handler [Entity Customer]
        getCustomers = io pool $ selectList [CustomerOwner ==. userKey] []

        postCustomer :: Customer -> Handler (Entity Customer)
        postCustomer postedCustomer = do
            payable <- io pool $ insert $ Account 0 (customerName postedCustomer ++ ": payable")
            assets <- io pool $ insert $ Account 0 (customerName postedCustomer ++ ": assets")
            let customer = postedCustomer { customerOwner = userKey, customerPayable = payable, customerAssets = assets }
            key <- io pool $ insert customer
            return $ Entity key customer 

        getCustomer :: CustomerId -> Handler (Maybe (Entity Customer))
        getCustomer customerId = do
            mCustomer <- io pool $ get customerId
            if checkOwner mCustomer userKey
                then return $ mCustomer >>= (Just . Entity customerId)
                else throwError err400

        putCustomer :: CustomerId -> Customer -> Handler (Maybe (Entity Customer))
        putCustomer customerId customer = do
            mOldCustomer <- io pool $ get customerId
            case mOldCustomer of
                Just oldCustomer -> if checkOwner mOldCustomer userKey
                    then do
                        let newCustomer = customer { customerOwner = userKey, customerPayable = customerPayable oldCustomer }
                        io pool $ replace customerId newCustomer
                        return $ Just $ Entity customerId newCustomer
                    else throwError err403
                _ -> throwError err404

        deleteCustomer :: CustomerId -> Handler String
        deleteCustomer customerId = do
            mOldCustomer <- io pool $ get customerId
            let ownerOk = checkOwner mOldCustomer userKey
            if ownerOk 
            then do
                io pool $ delete customerId 
                return ""
            else throwError err400
