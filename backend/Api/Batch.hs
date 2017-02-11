{-# LANGUAGE FlexibleInstances          #-}

module Api.Batch where

import Servant
import Database.Persist.Sql
import Control.Monad.IO.Class
import Control.Monad

import Model

type BatchApi = "batch"                                                       :> Get    '[JSON] [Entity Batch]
           :<|> "batch"                              :> ReqBody '[JSON] Batch :> Post   '[JSON] (Maybe (Entity Batch))
           :<|> "batch" :> Capture "key" (Key Batch)                          :> Get    '[JSON] (Maybe (Entity Batch))
           :<|> "batch" :> Capture "key" (Key Batch) :> ReqBody '[JSON] Batch :> Put    '[JSON] (Maybe (Entity Batch))
           :<|> "batch" :> Capture "key" (Key Batch)                          :> Delete '[JSON] String

batchServer :: ConnectionPool -> Entity User -> OrderId -> Server BatchApi
batchServer pool (Entity userId _) orderId = getBatches :<|> postBatch :<|> getBatch :<|> putBatch :<|> deleteBatch
    where
        io pool' action = liftIO $ runSqlPersistMPool action pool'

        checkOrder :: IO Bool
        checkOrder = do
            mOrder <- io pool $ get orderId
            return $ maybe False ((== userId) . orderOwner) mOrder

        checkStorage :: StorageId -> IO Bool
        checkStorage storageId = do
            mStorage <- io pool $ get storageId
            return $ maybe False ((== userId) . storageOwner) mStorage

        getBatches :: Handler [Entity Batch]
        getBatches = do
            orderOk <- liftIO checkOrder 
            if orderOk
            then io pool $ selectList [ BatchOrder ==. orderId ] []
            else throwError err403

        postBatch :: Batch -> Handler (Maybe (Entity Batch))
        postBatch batch = do
            orderOk <- liftIO checkOrder 
            storageOk <- liftIO $ checkStorage $ batchStorage batch
            if orderOk && storageOk
                then do
                    let newBatch = batch { batchOrder = orderId }
                    key <- io pool $ insert newBatch
                    return $ Just $ Entity key newBatch 
                else throwError err403

        getBatch :: Key Batch -> Handler (Maybe (Entity Batch))
        getBatch batchId = do
            mBatch <- io pool $ get batchId
            orderOk <- liftIO checkOrder 
            if orderOk
            then return $ do
                batch <- mBatch
                return $ Entity batchId batch
            else return Nothing

        putBatch :: Key Batch -> Batch -> Handler (Maybe (Entity Batch))
        putBatch batchId batch = do
            mOldBatch <- io pool $ get batchId
            case mOldBatch of
                Just _ -> do
                    orderOk <- liftIO checkOrder 
                    if orderOk 
                    then do
                        let newBatch = batch { batchOrder = orderId }
                        io pool $ replace batchId newBatch
                        return $ Just $ Entity batchId newBatch
                    else return Nothing
                _ -> return Nothing

        deleteBatch :: Key Batch -> Handler String
        deleteBatch batchId = do
            mOldBatch <- io pool $ get batchId
            _ <- case mOldBatch of
                Just _ -> do
                    orderOk <- liftIO checkOrder
                    when orderOk $ io pool $ delete batchId 
                _ -> throwError err404
            return ""
