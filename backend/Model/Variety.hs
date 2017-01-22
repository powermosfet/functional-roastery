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

module Model.Variety where

import Servant
import Database.Persist.Sql
import Database.Persist.TH
import Data.Aeson
import Data.Int
import Control.Monad.IO.Class
import Control.Monad

import UrlHelpers
import Model.User

share [mkPersist sqlSettings, mkMigrate "migrateVariety"] [persistLowerCase|
Variety
    country String
    description String
    owner UserId Maybe
    deriving Show
|]

instance FromJSON Variety where
  parseJSON = withObject "Variety" $ \ o -> do
        country <- o .: "country"
        description <- o .: "description"
        return $ Variety country description Nothing


instance ToJSON Variety where
    toJSON (Variety country description _) = object
        [ "country" .= country
        , "description" .= description
        ]

instance ToJSON (Entity Variety) where
    toJSON (Entity key o) = addEntityProps (toJSON o) key (entityUrl "variety")

instance HasOwner Variety where
    getOwner = varietyOwner

type VarietyApi = "variety" :> Get '[JSON] [Entity Variety]
             :<|> "variety" :> ReqBody '[JSON] Variety :> Post '[JSON] (Entity Variety)
             :<|> "variety" :> Capture "varietyId" Int64 :> Get '[JSON] (Maybe (Entity Variety))
             :<|> "variety" :> Capture "varietyId" Int64 :> ReqBody '[JSON] Variety :> Put '[JSON] (Maybe (Entity Variety))
             :<|> "variety" :> Capture "varietyId" (Key Variety) :> Delete '[JSON] String

varietyServer :: ConnectionPool -> Entity User -> Server VarietyApi
varietyServer pool user = getVarietiesH :<|> postVarietyH :<|> getVarietyH :<|> putVarietyH :<|> deleteVarietyH
    where
        getVarietiesH = liftIO (getVarieties pool user)
        postVarietyH variety = liftIO (postVariety pool user variety)
        getVarietyH varietyId = liftIO (getVariety pool user varietyId)
        putVarietyH varietyId variety = liftIO (putVariety pool user varietyId variety)
        deleteVarietyH varietyId = liftIO (deleteVariety pool user varietyId)

getVarieties :: ConnectionPool -> Entity User -> IO [Entity Variety]
getVarieties pool (Entity userKey _) = flip runSqlPersistMPool pool $
    selectList [VarietyOwner ==. Just userKey] []

postVariety :: ConnectionPool -> Entity User -> Variety -> IO (Entity Variety)
postVariety pool (Entity userKey _) v = flip runSqlPersistMPool pool $ do
    let variety = v { varietyOwner = Just userKey }
    key <- insert variety
    return $ Entity key variety

getVariety :: ConnectionPool -> Entity User -> Int64 -> IO (Maybe (Entity Variety))
getVariety pool (Entity userKey _) varietyId = flip runSqlPersistMPool pool $ do
    let varietyKey = toSqlKey varietyId
    mVariety <- get varietyKey
    if checkOwner mVariety userKey
        then return $ fmap (Entity varietyKey) mVariety
        else return Nothing

putVariety :: ConnectionPool -> Entity User -> Int64 -> Variety -> IO (Maybe (Entity Variety))
putVariety pool (Entity userKey _) varietyId variety = flip runSqlPersistMPool pool $ do
    let varietyKey = toSqlKey varietyId
    let newVariety = variety { varietyOwner = Just userKey }
    mOldVariety <- get varietyKey
    if checkOwner mOldVariety userKey
        then do
            replace varietyKey newVariety
            return $ Just $ Entity varietyKey newVariety
        else return Nothing

deleteVariety :: ConnectionPool -> Entity User -> Key Variety -> IO String
deleteVariety pool (Entity userKey _) varietyKey = flip runSqlPersistMPool pool $ do
    mOldVariety <- get varietyKey
    when (checkOwner mOldVariety userKey) $ delete varietyKey 
    return ""
