module Auth where

import Crypto.PasswordStore
import Database.Persist.Sql
import Servant
import Data.ByteString.Char8 as BS

import Model.User

authCheck :: ConnectionPool -> BasicAuthCheck User
authCheck pool =
    let
        check (BasicAuthData username password) = flip runSqlPersistMPool pool $ do
            mUser <- selectFirst [UserUsername ==. BS.unpack username] []
            case mUser of
                Just (Entity _ user@(User _ hash)) -> if verifyPassword password hash
                    then return (Authorized user)
                    else return BadPassword
                _ -> return Unauthorized
    in
        BasicAuthCheck check

basicAuthServerContext :: ConnectionPool -> Context (BasicAuthCheck User ': '[])
basicAuthServerContext pool = authCheck pool :. EmptyContext
