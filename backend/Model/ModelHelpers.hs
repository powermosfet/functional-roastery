{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Model.ModelHelpers where

import Data.Aeson
import qualified Data.HashMap.Strict as H

mergeObjects :: Value -> Value -> Value
mergeObjects (Object props1) (Object props2) = object $ H.toList $ H.union props1 props2
mergeObjects x _ = x

class BriefJSON a where
    toBriefJSON :: a -> Value
