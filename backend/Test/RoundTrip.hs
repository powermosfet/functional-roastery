{-# LANGUAGE FlexibleInstances          #-}

module RoundTrip (test)
    where
    
import Test.QuickCheck       (quickCheck, Gen, Arbitrary, arbitrary)
import Control.Monad         (ap)
import Database.Persist.Sql  (toSqlKey)
import Data.Aeson            (ToJSON, FromJSON, encode, decode)
import Model                 (UserId, CustomerId, StorageId, OrderId, Customer(..), Storage(..), Order(..), Batch(..))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Clock       (UTCTime)

newtype UserKey = UserKey 
    { getUserId :: UserId
    }

instance Arbitrary UserKey where
    arbitrary = UserKey . toSqlKey <$> arbitrary

newtype TestCustomer = TestCustomer Customer
    deriving (Show)

instance Arbitrary TestCustomer where
    arbitrary = TestCustomer <$> (Customer `fmap` arbitrary 
                                             `ap` arbitrary 
                                             `ap` arbitrary 
                                             `ap` fmap getUserId arbitrary
                                 )

roundTrip :: (ToJSON a, FromJSON a, Eq a) => a -> Bool
roundTrip x = case decode (encode x) of
    Just parsed -> parsed == x
    _ -> False

roundTripCustomer :: TestCustomer -> Bool
roundTripCustomer (TestCustomer customer) = roundTrip customer

newtype TestStorage = TestStorage Storage
    deriving (Show)

instance Arbitrary TestStorage where
    arbitrary = TestStorage <$> ( Storage `fmap` arbitrary 
                                            `ap` arbitrary 
                                            `ap` arbitrary 
                                            `ap` arbitrary 
                                            `ap` fmap getUserId arbitrary
                                )

roundTripStorage :: TestStorage -> Bool
roundTripStorage (TestStorage storage) = roundTrip storage

newtype CustomerKey = CustomerKey 
    { getCustomerId :: CustomerId
    }

instance Arbitrary CustomerKey where
    arbitrary = CustomerKey . toSqlKey <$> arbitrary

newtype TestOrder = TestOrder Order
    deriving (Show)

arbitraryTime :: Gen UTCTime
arbitraryTime = (posixSecondsToUTCTime . realToFrac) <$> (arbitrary :: Gen Int)

instance Arbitrary TestOrder where
    arbitrary = TestOrder <$> ( ( Order . getCustomerId ) `fmap` arbitrary 
                                                            `ap` arbitraryTime 
                                                            `ap` arbitrary 
                                                            `ap` fmap getUserId arbitrary
                              )

roundTripOrder :: TestOrder -> Bool
roundTripOrder (TestOrder order) = roundTrip order

newtype TestBatch = TestBatch Batch
    deriving (Show)

newtype OrderKey = OrderKey 
    { getOrderId :: OrderId
    }

instance Arbitrary OrderKey where
    arbitrary = OrderKey . toSqlKey <$> arbitrary

newtype StorageKey = StorageKey 
    { getStorageId :: StorageId
    }

instance Arbitrary StorageKey where
    arbitrary = StorageKey . toSqlKey <$> arbitrary

instance Arbitrary TestBatch where
    arbitrary = TestBatch <$> ( Batch `fmap` ( getOrderId <$> arbitrary ) 
                                        `ap` arbitraryTime 
                                        `ap` fmap getStorageId arbitrary
                                        `ap` arbitrary 
                                        `ap` arbitrary 
                                        `ap` arbitrary 
                              )

roundTripBatch :: TestBatch -> Bool
roundTripBatch (TestBatch batch) = roundTrip batch

test :: IO ()
test = do
    putStr "Rountrip check for Customer: " >> quickCheck roundTripCustomer
    putStr "Rountrip check for Storage:  " >> quickCheck roundTripStorage
    putStr "Rountrip check for Order:    " >> quickCheck roundTripOrder
    putStr "Rountrip check for Batch:    " >> quickCheck roundTripBatch
