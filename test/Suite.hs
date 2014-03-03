{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
module Main where

import Data.GELF
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Data.Aeson
import Data.Time.Clock
import Data.Default
import Data.Scientific
import Data.Time.Calendar
import Data.Text
import Data.Scientific
import Data.HashMap.Strict as HM
import Control.Applicative
import Data.Monoid

main :: IO ()
-- main = defaultMain tests
main = defaultMainWithOpts tests runnerOptions

runnerOptions :: RunnerOptions' Maybe
runnerOptions = mempty {
    ropt_test_options = Just testOptions
  }

testOptions :: TestOptions' Maybe
testOptions = mempty {
      topt_maximum_generated_tests = Just 1000
    , topt_maximum_unsuitable_generated_tests = Just 50
    , topt_maximum_test_size = Just 25
    , topt_maximum_test_depth = Just 1
  }

tests :: [Test]
tests = [testGroup "Parsing" [
    testProperty "round_trip" prop_roundtrip
  ]]

prop_roundtrip :: GELF -> Bool
prop_roundtrip x = (Just x) == decode (encode x)

instance Arbitrary GELF where
  arbitrary = do
    short <- arbitrary
    fullMsg <- arbitrary
    timestamp <- arbitrary
    level <- arbitrary
    facility <- arbitrary
    line <- arbitrary
    file <- arbitrary
    fields <- arbitrary
    return $ def { gelfShortMessage = short
                 , gelfFullMessage = fullMsg
                 , gelfTimestamp = timestamp
                 , gelfLevel = level
                 , gelfFacility = facility
                 , gelfLine = line
                 , gelfFile = file
                 , gelfAdditionalFields = fields
                 }

instance Arbitrary LogLevel where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary UTCTime where
  arbitrary = do
    day <- arbitrary
    time <- arbitrary
    return $ UTCTime (ModifiedJulianDay day) (secondsToDiffTime time)

instance Arbitrary Text where
  arbitrary = pack <$> listOf1 (choose (' ','~'))

instance Arbitrary AdditionalFields where
  arbitrary = fromList <$> listOf arbitraryKeyValue
    where
      arbitraryKeyValue :: Gen (Text,Field)
      arbitraryKeyValue = (,) <$> arbitraryKey <*> arbitrary 
      arbitraryKey :: Gen Text
      arbitraryKey = cons '_' <$> arbitrary

instance Arbitrary Field where
  arbitrary = oneof [ NumberField <$> arbitrary
                    , StringField <$> arbitrary
                    ]

instance Arbitrary Scientific where
  arbitrary = scientific <$> arbitrary <*> arbitrary