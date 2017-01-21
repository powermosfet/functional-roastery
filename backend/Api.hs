module Api where

import Servant
import Database.Persist.Sql
import Network.Wai

import Auth
import Model.User
import Model.Cat
import Config

type MyAPI = RegisterApi
        :<|> BasicAuth "my-realm" User :> UserApi
        :<|> BasicAuth "my-realm" User :> CatApi
        :<|> Raw

myAPI :: Proxy MyAPI
myAPI =
    Proxy

server :: ConnectionPool -> Server MyAPI
server pool = registerServer pool
         :<|> userServer pool
         :<|> catServer pool
         :<|> serveDirectory "static/"
 
mkApp :: Config -> IO Application
mkApp cfg = do
    pool <- makeDbPool cfg
    runSqlPool (runMigration migrateUser) pool
    runSqlPool (runMigration migrateCat) pool
    return $ app pool

app :: ConnectionPool -> Application
app pool = serveWithContext myAPI (basicAuthServerContext pool) (server pool)
