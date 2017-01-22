{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Model.User where

import Servant
import Data.ByteString.Char8 as BS
import Database.Persist.Sql
import Database.Persist.TH
import Data.Aeson
import Crypto.PasswordStore
import Control.Monad.IO.Class

import UrlHelpers

pwStrength :: Int
pwStrength = 19

share [mkPersist sqlSettings, mkMigrate "migrateUser"] [persistLowerCase|
User
    username String
    password BS.ByteString
    UniqueUsername username
    deriving Show
|]

instance ToJSON User where
  toJSON (User username _) =
    object [ "username" .= username ]

instance ToJSON (Entity User) where
    toJSON (Entity key user) = addEntityProps (toJSON user) key (entityUrl "user")

data NewUser = NewUser 
  { uapUsername :: String
  , uapPassword :: String
  }

instance FromJSON NewUser where
  parseJSON = withObject "NewUser" $ \ o -> do
        username <- o .: "username"
        password <- o .: "password"
        return $ NewUser username password

type RegisterApi = "user" :> "register" :> ReqBody '[JSON] NewUser :> Post '[JSON] (Maybe (Entity User))

registerServer :: ConnectionPool -> Server RegisterApi
registerServer pool = postUserH
    where
        postUserH user = liftIO (postUser pool user)

type UserApi = "user" :> "me" :> Get '[JSON] (Entity User)

userServer :: ConnectionPool -> Entity User -> Server UserApi
userServer _ user = liftIO (getUser user)

getUser :: Entity User -> IO (Entity User)
getUser = return

postUser :: ConnectionPool -> NewUser -> IO (Maybe (Entity User))
postUser pool (NewUser username password) = do
    hash <- makePassword (BS.pack password) pwStrength
    flip runSqlPersistMPool pool $ do
        let user = User username hash
        exists <- selectFirst [UserUsername ==. username] []
        case exists of
            Nothing -> do
                key <- insert user
                mPerson <- get key 
                case mPerson of
                    Just person -> return $ Just $ Entity key person
                    _ -> return Nothing
            Just _ -> return Nothing

class HasOwner a where
    getOwner :: a -> Maybe (Key User)

checkOwner :: (HasOwner a) => Maybe a -> Key User -> Bool
checkOwner o userKey = maybe False (== userKey) (o >>= getOwner) 
