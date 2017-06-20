{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Model where

import Database.Persist.TH
import Database.Persist.Sql
import Data.ByteString.Char8 as BS
import Data.Time.Clock
import Data.Aeson

import Model.ModelHelpers

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
   username String
   password BS.ByteString
   cash AccountId
   customersReceivable AccountId
   profit AccountId
   UniqueUsername username
   deriving Show Eq
Customer json
    name String
    email String
    payable AccountId
    assets AccountId
    owner UserId
    deriving Show Eq
Storage json
    country String
    variety String
    quantity Int
    value AccountId
    owner UserId 
    deriving Show Eq
Order json
    customer CustomerId
    timestamp UTCTime
    comment String
    owner UserId
    transactions [TransactionId]
    deriving Show Eq
Batch json
    order OrderId
    timestamp UTCTime
    storage StorageId
    quantity Int
    cost Double
    roastLevel String
    deriving Show Eq
Account json
    balance Double
    description String
    deriving Show Eq
Transaction json
    from AccountId
    to AccountId
    amount Double
    timestamp UTCTime
    deriving Show Eq
|]

class HasOwner a where
    getOwner :: a -> Key User
    setOwner :: Key User -> a -> a

checkOwner :: (HasOwner a, PersistEntity a) => Maybe a -> Key User -> Bool
checkOwner ob userKey = maybe False ((== userKey) . getOwner) ob  

instance ToJSON User where
    toJSON (User username _ _ _ _) =
        object [ "username" .= username ]

instance ToJSON (Entity User) where
    toJSON (Entity userId user) = mergeObjects (toJSON user) $ object [ "id" .= userId ]

instance HasOwner Storage where
    getOwner = storageOwner
    setOwner owner storage = storage { storageOwner = owner }

instance HasOwner Customer where
    getOwner = customerOwner
    setOwner owner customer = customer { customerOwner = owner }

instance HasOwner Order where
    getOwner = orderOwner
    setOwner owner order = order { orderOwner = owner }
