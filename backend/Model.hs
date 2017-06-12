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

import Model.Account.Class
import Model.ModelHelpers

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
   username String
   password BS.ByteString
   cashBalance Double
   UniqueUsername username
   deriving Show Eq
Customer json
    name String
    email String
    payable Double
    owner UserId
    deriving Show Eq
Storage json
    country String
    variety String
    quantity Int
    value Double
    owner UserId 
    deriving Show Eq
Order json
    customer CustomerId
    timestamp UTCTime
    comment String
    owner UserId
    deriving Show Eq
Batch json
    order OrderId
    timestamp UTCTime
    storage StorageId
    quantity Int
    cost Double
    roastLevel String
    deriving Show Eq
|]

class HasOwner a where
    getOwner :: a -> Key User
    setOwner :: Key User -> a -> a

checkOwner :: (HasOwner a, PersistEntity a) => Maybe a -> Key User -> Bool
checkOwner ob userKey = maybe False ((== userKey) . getOwner) ob  

instance ToJSON User where
    toJSON (User username _ _) =
        object [ "username" .= username ]

instance ToJSON (Entity User) where
    toJSON (Entity userId user) = mergeObjects (toJSON user) $ object [ "id" .= userId ]

instance Account User where
    toAccountView User { userUsername = username, userCashBalance = balance } =
        AccountView
            { accountName    = username ++ " - Cash account"
            , accountBalance = balance
            }
    withdraw amount u@User { userCashBalance = balance } =
        u { userCashBalance = balance - amount }
    deposit amount u@User { userCashBalance = balance } =
        u { userCashBalance = balance + amount }

instance HasOwner Storage where
    getOwner = storageOwner
    setOwner owner storage = storage { storageOwner = owner }

instance Account Storage where
    toAccountView Storage
        { storageCountry = country
        , storageVariety = variety 
        , storageValue = balance 
        } = AccountView
            { accountName    
                =  country 
                ++ ", "
                ++ variety
                ++ " - Storage account"
            , accountBalance = balance
            }
    withdraw amount u@Storage { storageValue = balance } =
        u { storageValue = balance - amount }
    deposit amount u@Storage { storageValue = balance } =
        u { storageValue = balance + amount }

instance HasOwner Customer where
    getOwner = customerOwner
    setOwner owner customer = customer { customerOwner = owner }

instance Account Customer where
    toAccountView Customer { customerName = name, customerPayable = balance } =
        AccountView
            { accountName    = name ++ " - Customer payable account"
            , accountBalance = balance
            }
    withdraw amount u@Customer { customerPayable = balance } =
        u { customerPayable = balance - amount }
    deposit amount u@Customer { customerPayable = balance } =
        u { customerPayable = balance + amount }

instance HasOwner Order where
    getOwner = orderOwner
    setOwner owner order = order { orderOwner = owner }
