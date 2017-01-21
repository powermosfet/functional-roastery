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

module Model.Cat where

import Servant
import Database.Persist.Sql
import Database.Persist.TH
import Data.Aeson
import Control.Monad.IO.Class

import UrlHelpers
import Model.User

share [mkPersist sqlSettings, mkMigrate "migrateCat"] [persistLowerCase|
Cat 
    name String
    UniqueName name
    deriving Show
|]

instance FromJSON Cat where
    parseJSON = withObject "Cat" $ \ v ->
        Cat <$> v .: "name"

instance ToJSON Cat where
    toJSON (Cat name) = object
        [ "name" .= name
        ]

instance ToJSON (Entity Cat) where
    toJSON (Entity key cat) = addEntityProps (toJSON cat) key (entityUrl "cat")

type CatApi = "cat" :> Get '[JSON] [Entity Cat]
         :<|> "cat" :> ReqBody '[JSON] Cat :> Post '[JSON] (Maybe (Key Cat))

catServer :: ConnectionPool -> User -> Server CatApi
catServer pool user = getCatsH :<|> postCatH
    where
        getCatsH  = liftIO (getCats pool)
        postCatH cat = liftIO (postCat pool cat)

getCats :: ConnectionPool -> IO [Entity Cat]
getCats pool = flip runSqlPersistMPool pool $ selectList [] []

postCat :: ConnectionPool -> Cat -> IO (Maybe (Key Cat))
postCat pool cat = flip runSqlPersistMPool pool $ do
    exists <- selectFirst [CatName ==. catName cat] []
    case exists of
        Nothing -> Just <$> insert cat
        Just _ -> return Nothing
