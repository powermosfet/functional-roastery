{-# LANGUAGE FlexibleInstances          #-}

module Api.Storage where

import Servant
import Database.Persist.Sql
import Control.Monad.IO.Class
import Control.Monad

import Model

type StorageApi = "storage" :> Get '[JSON] [Entity Storage]
             :<|> "storage" :> Post '[JSON] (Entity Storage)
             :<|> "storage" :> Capture "storageId" (Key Storage) :> Get '[JSON] (Maybe (Entity Storage))
             :<|> "storage" :> Capture "storageId" (Key Storage) :> ReqBody '[JSON] Storage :> Put '[JSON] (Maybe (Entity Storage))
             :<|> "storage" :> Capture "storageId" (Key Storage) :> Delete '[JSON] String

storageServer :: ConnectionPool -> Entity User -> Server StorageApi
storageServer pool user = getListH :<|> createH :<|> getSingleH :<|> changeH :<|> deleteH
    where
        getListH                  = liftIO (getStorages   pool user)
        createH                   = liftIO (postStorage   pool user)
        getSingleH storageId      = liftIO (getStorage    pool user storageId)
        changeH storageId storage = liftIO (putStorage    pool user storageId storage)
        deleteH storageId         = liftIO (deleteStorage pool user storageId)

getStorages :: ConnectionPool -> Entity User -> IO [Entity Storage]
getStorages pool (Entity userKey _) = flip runSqlPersistMPool pool $
    selectList [StorageOwner ==. userKey] []

postStorage :: ConnectionPool -> Entity User -> IO (Entity Storage)
postStorage pool (Entity userKey _) = flip runSqlPersistMPool pool $ do
    value <- insert $ Account 0 "Storage value"
    let storage = Storage "" "" 0 value userKey 
    key <- insert storage
    return $ Entity key storage

getStorage :: ConnectionPool -> Entity User -> Key Storage -> IO (Maybe (Entity Storage))
getStorage pool (Entity userKey _) storageKey = flip runSqlPersistMPool pool $ do
    mStorage <- get storageKey
    if checkOwner mStorage userKey
        then return $ fmap (Entity storageKey) mStorage
        else return Nothing

putStorage :: ConnectionPool -> Entity User -> Key Storage -> Storage -> IO (Maybe (Entity Storage))
putStorage pool (Entity userKey _) storageKey storage = flip runSqlPersistMPool pool $ do
    mOldStorage <- get storageKey
    case mOldStorage of
        Just (Storage _ _ oldQuantity oldValue _) ->
            if checkOwner mOldStorage userKey
                then do
                    let newStorage = storage { storageOwner = userKey, storageQuantity = oldQuantity, storageValue = oldValue }
                    replace storageKey newStorage
                    return $ Just $ Entity storageKey newStorage
                else return Nothing
        _ -> return Nothing

deleteStorage :: ConnectionPool -> Entity User -> Key Storage -> IO String
deleteStorage pool (Entity userKey _) storageKey = flip runSqlPersistMPool pool $ do
    mOldStorage <- get storageKey
    when (checkOwner mOldStorage userKey) $ delete storageKey 
    return ""
