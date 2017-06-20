module RoundTrip (test)
    where
    
import Test.QuickCheck       (quickCheck, Gen, Arbitrary, arbitrary)
import Control.Monad         (ap)
import Database.Persist.Sql  (toSqlKey)
import Data.Aeson            (ToJSON, FromJSON, encode, decode)
import Model                 (Customer(..), Storage(..), Order(..), Batch(..))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Clock       (UTCTime)

newtype TestCustomer = TestCustomer Customer
    deriving (Show)

instance Arbitrary TestCustomer where
    arbitrary = TestCustomer <$> ( ( Customer <$> arbitrary )
                                             `ap` arbitrary 
                                             `ap` ( toSqlKey <$> arbitrary )
                                             `ap` ( toSqlKey <$> arbitrary )
                                             `ap` ( toSqlKey <$> arbitrary )
                                 )
(|>) ::  a -> (a -> b) -> b
(|>) = flip ($)

roundTrip :: (ToJSON a, FromJSON a, Eq a) => a -> Bool
roundTrip x = encode x
                |> decode
                |> maybe False (== x)

roundTripCustomer :: TestCustomer -> Bool
roundTripCustomer (TestCustomer customer) = roundTrip customer

newtype TestStorage = TestStorage Storage
    deriving (Show)

instance Arbitrary TestStorage where
    arbitrary = TestStorage <$> ( ( Storage <$> arbitrary ) 
                                            `ap` arbitrary 
                                            `ap` arbitrary 
                                            `ap` ( toSqlKey <$> arbitrary )
                                            `ap` ( toSqlKey <$> arbitrary )
                                )

roundTripStorage :: TestStorage -> Bool
roundTripStorage (TestStorage storage) = roundTrip storage

newtype TestOrder = TestOrder Order
    deriving (Show)

arbitraryTime :: Gen UTCTime
arbitraryTime = (posixSecondsToUTCTime . realToFrac) <$> (arbitrary :: Gen Int)

instance Arbitrary TestOrder where
    arbitrary = TestOrder <$> ( ( Order <$> ( toSqlKey <$> arbitrary ) )
                                                      `ap` arbitraryTime 
                                                      `ap` arbitrary 
                                                      `ap` ( toSqlKey <$> arbitrary )
                                                      `ap` ( (toSqlKey <$>) <$> arbitrary )
                              )

roundTripOrder :: TestOrder -> Bool
roundTripOrder (TestOrder order) = roundTrip order

newtype TestBatch = TestBatch Batch
    deriving (Show)

instance Arbitrary TestBatch where
    arbitrary = TestBatch <$> ( ( Batch <$> ( toSqlKey <$> arbitrary ) )
                                       `ap` arbitraryTime 
                                       `ap` ( toSqlKey <$> arbitrary )
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
