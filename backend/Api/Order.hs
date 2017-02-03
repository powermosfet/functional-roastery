{-# LANGUAGE FlexibleInstances          #-}

module Api.Order where

import Servant
import Database.Persist.Sql
import Control.Monad.IO.Class
import Control.Monad

import Model
import Api.Batch

type OrderApi = "order" :> Get '[JSON] [Entity Order]
           :<|> "order" :> ReqBody '[JSON] Order :> Post '[JSON] (Entity Order)
           :<|> "order" :> Capture "key" (Key Order) :> Get '[JSON] (Maybe (Entity Order))
           :<|> "order" :> Capture "key" (Key Order) :> ReqBody '[JSON] Order :> Put '[JSON] (Maybe (Entity Order))
           :<|> "order" :> Capture "key" (Key Order) :> Delete '[JSON] String
           :<|> "order" :> Capture "key" (Key Order) :> BatchApi

orderServer :: ConnectionPool -> Entity User -> Server OrderApi
orderServer pool user = getListH :<|> createH :<|> getSingleH :<|> changeH :<|> deleteH :<|> batchServerH
    where
        getListH             = liftIO (getOrders   pool user)
        createH order        = liftIO (postOrder   pool user order)
        getSingleH key       = liftIO (getOrder    pool user key)
        changeH key order    = liftIO (putOrder    pool user key order)
        deleteH key          = liftIO (deleteOrder pool user key)
        batchServerH         = batchServer pool user

getOrders :: ConnectionPool -> Entity User -> IO [Entity Order]
getOrders pool (Entity userKey _) = flip runSqlPersistMPool pool $
    selectList [OrderOwner ==. userKey] []

postOrder :: ConnectionPool -> Entity User -> Order -> IO (Entity Order)
postOrder pool (Entity userKey _) order = flip runSqlPersistMPool pool $ do
    let newOrder = order { orderOwner = userKey }
    key <- insert newOrder
    return $ Entity key newOrder 

getOrder :: ConnectionPool -> Entity User -> Key Order -> IO (Maybe (Entity Order))
getOrder pool (Entity userKey _) orderKey = do
    mOrder <- flip runSqlPersistMPool pool $ get orderKey
    let ownerOk = checkOwner mOrder userKey
    if ownerOk
        then return $ mOrder >>= (Just . Entity orderKey)
        else return Nothing

putOrder :: ConnectionPool -> Entity User -> Key Order -> Order -> IO (Maybe (Entity Order))
putOrder pool (Entity userKey _) orderKey order = do 
    mOldOrder <- flip runSqlPersistMPool pool $ get orderKey
    case mOldOrder of
        Just _ -> if checkOwner mOldOrder userKey
            then do
                let newOrder = order { orderOwner = userKey }
                flip runSqlPersistMPool pool $ replace orderKey newOrder
                return $ Just $ Entity orderKey newOrder
            else return Nothing
        _ -> return Nothing

deleteOrder :: ConnectionPool -> Entity User -> Key Order -> IO String
deleteOrder pool (Entity userKey _) orderKey = do
    mOldOrder <- flip runSqlPersistMPool pool $ get orderKey
    let ownerOk = checkOwner mOldOrder userKey
    when ownerOk $ flip runSqlPersistMPool pool $ delete orderKey 
    return ""
