{-# LANGUAGE OverloadedStrings #-}
module Data.GELF where

import Data.Text
import Data.Aeson
import Data.Aeson.Types
import Data.Time.Clock
import Data.Time.Format
import Data.HashMap as HM
import Data.Scientific
import Data.Maybe
import System.Locale
import Data.Default
import Control.Applicative
import System.IO.Unsafe
import Control.Monad (mzero)

data LogLevel = Emergency | Alert | Critical | Error | Warning | Notice | Informational | Debug
  deriving (Enum, Show, Eq)

instance FromJSON LogLevel where
  parseJSON n = toEnum <$> parseJSON n

data Field = NumberField !Scientific | StringField !Text
  deriving (Show, Eq)

type AdditionalFields = Map Text Field

data GELF = GELF { gelfVersion :: !Text
                 , gelfHost :: !Text
                 , gelfShortMessage :: Text
                 , gelfFullMessage :: Maybe Text
                 , gelfTimestamp :: Maybe UTCTime
                 , gelfLevel :: Maybe LogLevel
                 , gelfFacility :: Maybe Text
                 , gelfLine :: Maybe Int
                 , gelfFile :: Maybe Text
                 , gelfAdditionalFields :: AdditionalFields
                 } deriving (Show)

instance Default GELF where
  def = GELF "1.1" "GELF" "Unknown" Nothing (Just now) (Just Alert) Nothing Nothing Nothing HM.empty
    where
      now = unsafePerformIO getCurrentTime

formatPosixTime :: FormatTime t => t -> String
formatPosixTime = formatTime defaultTimeLocale "%s%Q"

instance ToJSON GELF where
  toJSON g = object $ (catMaybes [ Just ("version" .= unpack (gelfVersion g))
                                 , Just ("host" .= unpack (gelfHost g))
                                 , Just ("short_message" .= unpack (gelfShortMessage g))
                                 , (.=) "long_message" . unpack <$> gelfFullMessage g
                                 , (.=) "timestamp" . formatPosixTime <$> gelfTimestamp g
                                 , (.=) "level" . fromEnum <$> gelfLevel g
                                 , (.=) "facility" . unpack <$> gelfFacility g
                                 , (.=) "line" . show <$> gelfLine g
                                 , (.=) "file" . unpack <$> gelfFile g])
                      ++ fieldsToPairs (gelfAdditionalFields g)


-- | Converts a key name and value into a Pair appropriate for feeding to object
--
-- >>> fieldToPair (pack "Test") (StringField (pack "Testing"))
-- ("_Test",String "Testing")
-- >>> fieldToPair (pack "Test2") (NumberField 3.14159)
-- ("_Test2",Number 3.14159)
fieldToPair :: Text -> Field -> Pair
fieldToPair name (NumberField x) = (cons '_' name) .= x
fieldToPair name (StringField x) = (cons '_' name) .= (unpack x)

-- | Converts AdditionalFields to a list of Pair suitable for feeding to object
--
-- >>> fieldsToPairs (fromList [(pack "Test", StringField $ pack "Testing"),(pack "Test2", NumberField 3.14159)])
-- [("_Test",String "Testing"),("_Test2",Number 3.14159)]
fieldsToPairs :: AdditionalFields -> [Pair]
fieldsToPairs fields = foldWithKey f [] fields
  where
    f key val xs = xs ++ [fieldToPair key val]

instance FromJSON GELF where
  parseJSON (Object v) = GELF <$>
                         v .: "version" <*>
                         v .: "host" <*>
                         v .: "short_message" <*>
                         v .:? "long_message" <*>
                         v .:? "timestamp" <*>
                         v .:? "level" <*>
                         v .:? "facility" <*>
                         v .:? "line" <*>
                         v .:? "file" <*>
                         parseRest v
  parseJSON _          = mzero

parseRest :: Object -> Parser AdditionalFields
parseRest m = undefined -- TODO