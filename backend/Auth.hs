module Auth where

import Crypto.PasswordStore
import Database.Persist.Sql
import Servant
import Data.ByteString.Char8 as BS

import Model

authCheck :: ConnectionPool -> BasicAuthCheck (Entity User)
authCheck pool =
    let
        check (BasicAuthData username password) = flip runSqlPersistMPool pool $ do
            mUser <- selectFirst [UserUsername ==. BS.unpack username] []
            case mUser of
                Just user@(Entity _ (User _ hash _ _ _)) -> if verifyPassword password hash
                    then return (Authorized user)
                    else return BadPassword
                _ -> return Unauthorized
    in
        BasicAuthCheck check

basicAuthServerContext :: ConnectionPool -> Context (BasicAuthCheck (Entity User) ': '[])
basicAuthServerContext pool = authCheck pool :. EmptyContext
