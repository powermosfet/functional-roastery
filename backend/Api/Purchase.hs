{-# LANGUAGE FlexibleInstances          #-}

module Api.Purchase where

import Servant
import Database.Persist.Sql
import Control.Monad.IO.Class
import Control.Monad

import Model

type PurchaseApi = "purchase"                                                             :> Get    '[JSON] [Entity Purchase]
              :<|> "purchase"                                 :> ReqBody '[JSON] Purchase :> Post   '[JSON] (Maybe (Entity Purchase))
              :<|> "purchase" :> Capture "key" (Key Purchase)                             :> Get    '[JSON] (Maybe (Entity Purchase))
              :<|> "purchase" :> Capture "key" (Key Purchase) :> ReqBody '[JSON] Purchase :> Put    '[JSON] (Maybe (Entity Purchase))
              :<|> "purchase" :> Capture "key" (Key Purchase)                             :> Delete '[JSON] String

purchaseServer :: ConnectionPool -> Entity User -> OrderId -> Server PurchaseApi
purchaseServer pool (Entity userId _) orderId = getPurchasees :<|> postPurchase :<|> getPurchase :<|> putPurchase :<|> deletePurchase
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

        getPurchasees :: Handler [Entity Purchase]
        getPurchasees = do
            orderOk <- liftIO checkOrder 
            if orderOk
            then io pool $ selectList [ PurchaseOrder ==. orderId ] []
            else throwError err403

        postPurchase :: Purchase -> Handler (Maybe (Entity Purchase))
        postPurchase purchase = do
            orderOk <- liftIO checkOrder 
            storageOk <- liftIO $ checkStorage $ purchaseStorage purchase
            if orderOk && storageOk
                then do
                    let newPurchase = purchase { purchaseOrder = orderId }
                    key <- io pool $ insert newPurchase
                    return $ Just $ Entity key newPurchase 
                else throwError err403

        getPurchase :: Key Purchase -> Handler (Maybe (Entity Purchase))
        getPurchase purchaseId = do
            mPurchase <- io pool $ get purchaseId
            orderOk <- liftIO checkOrder 
            if orderOk
            then return $ do
                purchase <- mPurchase
                return $ Entity purchaseId purchase
            else return Nothing

        putPurchase :: Key Purchase -> Purchase -> Handler (Maybe (Entity Purchase))
        putPurchase purchaseId purchase = do
            mOldPurchase <- io pool $ get purchaseId
            case mOldPurchase of
                Just _ -> do
                    orderOk <- liftIO checkOrder 
                    if orderOk 
                    then do
                        let newPurchase = purchase { purchaseOrder = orderId }
                        io pool $ replace purchaseId newPurchase
                        return $ Just $ Entity purchaseId newPurchase
                    else return Nothing
                _ -> return Nothing

        deletePurchase :: Key Purchase -> Handler String
        deletePurchase purchaseId = do
            mOldPurchase <- io pool $ get purchaseId
            _ <- case mOldPurchase of
                Just _ -> do
                    orderOk <- liftIO checkOrder
                    when orderOk $ io pool $ delete purchaseId 
                _ -> throwError err404
            return ""
