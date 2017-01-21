{-# LANGUAGE FlexibleContexts           #-}

module UrlHelpers where

import Database.Persist.Sql
import Data.Aeson
import qualified Data.HashMap.Strict as H
import Data.List
import Data.Int

entityUrl :: (Show a) => String -> a -> String
entityUrl namespace key = intercalate "/" [ "", namespace, show key ]

addEntityProps :: (ToBackendKey SqlBackend a) => Value -> Key a -> (Int64 -> String) -> Value
addEntityProps ob key fUrl = 
    let
        obId = fromSqlKey key
    in
        mergeObjects
            ob $ object
                [ "id" .= obId
                , "link" .= fUrl obId
                ]

mergeObjects :: Value -> Value -> Value
mergeObjects (Object props1) (Object props2) = object $ H.toList $ H.union props1 props2
mergeObjects x _ = x

