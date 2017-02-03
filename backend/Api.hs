module Api where

import Servant
import Database.Persist.Sql
import Network.Wai

import Auth
import Model
import Api.User
import Api.Storage
import Api.Customer
import Api.Account
import Api.Order
import Config

type MyAuth = BasicAuth "my-realm" (Entity User)

type MyAPI = RegisterApi
        :<|> MyAuth :> UserApi
        :<|> MyAuth :> StorageApi
        :<|> MyAuth :> CustomerApi
        :<|> MyAuth :> AccountApi
        :<|> MyAuth :> OrderApi
        :<|> Raw

myAPI :: Proxy MyAPI
myAPI =
    Proxy

server :: ConnectionPool -> Server MyAPI
server pool = registerServer pool
         :<|> userServer pool
         :<|> storageServer pool
         :<|> customerServer pool
         :<|> accountServer pool
         :<|> orderServer pool
         :<|> serveDirectory "static/"
 
mkApp :: Config -> IO Application
mkApp cfg = do
    pool <- makeDbPool cfg
    runSqlPool (runMigration migrateAll) pool
    return $ app pool

app :: ConnectionPool -> Application
app pool = serveWithContext myAPI (basicAuthServerContext pool) (server pool)

