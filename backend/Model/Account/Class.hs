module Model.Account.Class where

import Data.Aeson

data AccountView = AccountView
    { accountName :: String
    , accountBalance :: Double
    }

class Account a where
    toAccountView :: a -> AccountView
    withdraw :: Double -> a -> a
    deposit :: Double -> a -> a

instance ToJSON AccountView where
    toJSON (AccountView name balance) = object
        [ "name" .= name
        , "balance" .= balance
        ]

