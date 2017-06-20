{-# LANGUAGE FlexibleInstances          #-}

module Api.User where

import Servant
import Data.ByteString.Char8 as BS
import Database.Persist.Sql
import Data.Aeson
import Crypto.PasswordStore
import Control.Monad.IO.Class

import Model

pwStrength :: Int
pwStrength = 19

data NewUser = NewUser 
    { uapUsername :: String
    , uapPassword :: String
    }

instance FromJSON NewUser where
    parseJSON = withObject "NewUser" $ \ o -> do
        username <- o .: "username"
        password <- o .: "password"
        return $ NewUser username password

type RegisterApi = "user" :> "register" :> ReqBody '[JSON] NewUser :> Post '[JSON] (Maybe User)

registerServer :: ConnectionPool -> Server RegisterApi
registerServer pool = postUserH
    where
        postUserH user = liftIO (postUser pool user)

type UserApi = "user" :> "me" :> Get '[JSON] (Entity User)

userServer :: ConnectionPool -> Entity User -> Server UserApi
userServer _ user = liftIO (getUser user)

getUser :: Entity User -> IO (Entity User)
getUser = return

postUser :: ConnectionPool -> NewUser -> IO (Maybe User)
postUser pool (NewUser username password) = do
    hash <- makePassword (BS.pack password) pwStrength
    flip runSqlPersistMPool pool $ do
        cash <- insert $ Account 0 (username ++ ": cash")
        receivable <- insert $ Account 0 (username ++ ": accounts receivable")
        profit <- insert $ Account 0 (username ++ ": profit")
        let user = User username hash cash receivable profit
        exists <- selectFirst [UserUsername ==. username] []
        case exists of
            Nothing -> do
                _ <- insert user
                return $ Just user
            Just _ -> return Nothing

