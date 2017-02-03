{-# LANGUAGE FlexibleInstances          #-}

module Api.Batch where

import Servant
import Database.Persist.Sql
import Control.Monad.IO.Class
import Control.Monad

import Model

type BatchApi = "batch" :> Get '[JSON] [Entity Batch]
           :<|> "batch" :> ReqBody '[JSON] Batch :> Post '[JSON] (Maybe (Entity Batch))
           :<|> "batch" :> Capture "key" (Key Batch) :> Get '[JSON] (Maybe (Entity Batch))
           :<|> "batch" :> Capture "key" (Key Batch) :> ReqBody '[JSON] Batch :> Put '[JSON] (Maybe (Entity Batch))
           :<|> "batch" :> Capture "key" (Key Batch) :> Delete '[JSON] String

batchServer :: ConnectionPool -> Entity User -> OrderId -> Server BatchApi
batchServer pool user order = getListH :<|> createH :<|> getSingleH :<|> changeH :<|> deleteH
    where
        getListH             = liftIO (getBatches  pool user order)
        createH obj          = liftIO (postBatch   pool user order obj)
        getSingleH key       = liftIO (getBatch    pool user order key)
        changeH key obj      = liftIO (putBatch    pool user order key obj)
        deleteH key          = liftIO (deleteBatch pool user order key)

checkOrder :: ConnectionPool -> UserId -> OrderId -> IO Bool
checkOrder pool userId orderId = liftIO $ flip runSqlPersistMPool pool $ do
    mOrder <- get orderId
    return $ maybe False ((== userId) . orderOwner) mOrder

checkStorage :: ConnectionPool -> UserId -> StorageId -> IO Bool
checkStorage pool userId storageId = liftIO $ flip runSqlPersistMPool pool $ do
    mStorage <- get storageId
    return $ maybe False ((== userId) . storageOwner) mStorage

getBatches :: ConnectionPool -> Entity User -> OrderId -> IO [Entity Batch]
getBatches pool (Entity userId _) orderId = do
    orderOk <- checkOrder pool userId orderId
    flip runSqlPersistMPool pool $
        if orderOk
        then selectList [ BatchOrder ==. orderId ] []
        else return []

postBatch :: ConnectionPool -> Entity User -> OrderId -> Batch -> IO (Maybe (Entity Batch))
postBatch pool (Entity userId _) orderId batch = do
    orderOk <- checkOrder pool userId orderId
    storageOk <- checkStorage pool userId $ batchStorage batch
    flip runSqlPersistMPool pool $
        if orderOk && storageOk
        then do
            let newBatch = batch { batchOrder = orderId }
            key <- insert newBatch
            return $ Just $ Entity key newBatch 
        else return Nothing

getBatch :: ConnectionPool -> Entity User -> OrderId -> Key Batch -> IO (Maybe (Entity Batch))
getBatch pool (Entity userId _) _ batchId = do
    mBatch <- flip runSqlPersistMPool pool $ get batchId
    case mBatch of
        (Just batch) -> do
            orderOk <- checkOrder pool userId (batchOrder batch)
            if orderOk
            then return $ mBatch >>= (Just . Entity batchId)
            else return Nothing
        _ -> return Nothing

putBatch :: ConnectionPool -> Entity User -> OrderId -> Key Batch -> Batch -> IO (Maybe (Entity Batch))
putBatch pool (Entity userId _) orderId batchId batch = do
    mOldBatch <- flip runSqlPersistMPool pool $ get batchId
    case mOldBatch of
        Just oldBatch -> do
            orderOk <- checkOrder pool userId (batchOrder oldBatch) 
            if orderOk 
            then do
                let newBatch = batch { batchOrder = orderId }
                flip runSqlPersistMPool pool $ replace batchId newBatch
                return $ Just $ Entity batchId newBatch
            else return Nothing
        _ -> return Nothing

deleteBatch :: ConnectionPool -> Entity User -> OrderId -> Key Batch -> IO String
deleteBatch pool (Entity userId _) _ batchId = do
    mOldBatch <- flip runSqlPersistMPool pool $ get batchId
    _ <- case mOldBatch of
        Just oldBatch -> do
            orderOk <- checkOrder pool userId (batchOrder oldBatch)
            when orderOk $ flip runSqlPersistMPool pool $ delete batchId 
        _ -> return ()
    return ""
