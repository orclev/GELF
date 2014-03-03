{-# LANGUAGE OverloadedStrings #-}
module Data.GELF ( GELF (..)
                 , LogLevel (..)
                 , Field (..)
                 , AdditionalFields
                 )where

import Data.Text
import Data.Aeson
import Data.Aeson.Types
import Data.Time.Clock
import Data.Time.Format
import Data.HashMap.Strict as HM
import Data.Scientific
import Data.Maybe
import System.Locale
import Data.Default
import Control.Applicative
import System.IO.Unsafe
import Control.Monad (mzero)

data LogLevel = Emergency | Alert | Critical | Error | Warning | Notice | Informational | Debug
  deriving (Enum, Show, Eq, Bounded)

instance FromJSON LogLevel where
  parseJSON n = toEnum <$> parseJSON n

data Field = NumberField !Scientific | StringField !Text
  deriving (Show, Eq)

type AdditionalFields = HashMap Text Field

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

instance Eq GELF where
  (==) a b = gelfVersion a == gelfVersion b
             && gelfHost a == gelfHost b
             && gelfShortMessage a == gelfShortMessage b
             && gelfFullMessage a == gelfFullMessage b
             && gelfLevel a == gelfLevel b
             && gelfFacility a == gelfFacility b
             && gelfLine a == gelfLine b
             && gelfFile a == gelfFile b
             -- && gelfTimestamp a == gelfTimestamp b
             -- && gelfAdditionalFields a == gelfAdditionalFields b

instance Default GELF where
  def = GELF "1.1" "GELF" "Unknown" Nothing (Just now) (Just Alert) Nothing Nothing Nothing HM.empty
    where
      now = unsafePerformIO getCurrentTime

formatPosixTime :: FormatTime t => t -> String
formatPosixTime = formatTime defaultTimeLocale "%s%Q"

parsePosixTime :: ParseTime t => String -> Maybe t
parsePosixTime = parseTime defaultTimeLocale "%s%Q"

maybeParsePosixTime :: ParseTime t => Maybe String -> Maybe t
maybeParsePosixTime s = s >>= parsePosixTime

instance ToJSON GELF where
  toJSON g = object $ (catMaybes [ Just ("version" .= unpack (gelfVersion g))
                                 , Just ("host" .= unpack (gelfHost g))
                                 , Just ("short_message" .= unpack (gelfShortMessage g))
                                 , (.=) "long_message" . unpack <$> gelfFullMessage g
                                 , (.=) "timestamp" . formatPosixTime <$> gelfTimestamp g
                                 , (.=) "level" . fromEnum <$> gelfLevel g
                                 , (.=) "facility" . unpack <$> gelfFacility g
                                 , (.=) "line" <$> gelfLine g
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
fieldsToPairs fields = foldrWithKey f [] fields
  where
    f key val xs = xs ++ [fieldToPair key val]

instance FromJSON GELF where
  parseJSON (Object v) = GELF <$>
                         v .: "version" <*>
                         v .: "host" <*>
                         v .: "short_message" <*>
                         v .:? "long_message" <*>
                         (maybeParsePosixTime <$> (v .:? "timestamp")) <*>
                         v .:? "level" <*>
                         v .:? "facility" <*>
                         v .:? "line" <*>
                         v .:? "file" <*>
                         parseRest v
  parseJSON _          = mzero

parseRest :: Object -> Parser AdditionalFields
parseRest m = pure $ HM.map f2 $ filterWithKey f m
  where
    f key _ = isPrefixOf "_" key
    f2 (String t) = StringField t
    f2 (Number n) = NumberField n
    f2 _ = undefined