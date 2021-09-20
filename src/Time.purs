module Hours.Time
  ( Instant
  , getNow
  , asMilliseconds
  , isToday
  , Minutes(..)
  , minutesBetween
  , parseMinutes
  ) where

import Prelude

import Effect (Effect)
import Data.Int (round)
import Data.Nullable (Nullable, toMaybe)
import Data.Maybe (Maybe)
import Data.Either (note)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Decode (JsonDecodeError(TypeMismatch)) as A

newtype Instant = Instant { millis :: Number }

derive instance Eq Instant
derive instance Ord Instant

foreign import getNow_f
  :: (Number -> Instant)
  -> Effect Instant

getNow :: Effect Instant
getNow = getNow_f \millis -> Instant { millis }

asMilliseconds :: Instant -> Number
asMilliseconds (Instant { millis }) = millis

foreign import isToday :: Instant -> Effect Boolean

newtype Minutes = Minutes Int

derive instance Eq Minutes
derive instance Ord Minutes

instance Semigroup Minutes where
  append (Minutes n) (Minutes m) = Minutes $ m + n

instance Monoid Minutes where
  mempty = Minutes 0

foreign import parseMinutes_f :: String -> Nullable Minutes

parseMinutes :: String -> Maybe Minutes
parseMinutes = toMaybe <$> parseMinutes_f

minutesBetween :: Instant -> Instant -> Minutes
minutesBetween a b = Minutes <<< round $ (toMillis b - toMillis a) `div` (1000.0 * 60.0)
  where toMillis (Instant { millis }) = millis


-- aeson instnaces --

foreign import printMillis :: Number -> String
foreign import parseMillis :: String -> Nullable Number

instance EncodeJson Instant where
  encodeJson (Instant { millis }) = encodeJson $ printMillis millis

instance DecodeJson Instant where
  decodeJson json = do
    str <- decodeJson json
    millis <- parseMillis str # toMaybe # note (A.TypeMismatch "Expected datetime")
    pure $ Instant { millis }

instance EncodeJson Minutes where
  encodeJson (Minutes n) = encodeJson n

instance DecodeJson Minutes where
  decodeJson json = Minutes <$> decodeJson json
