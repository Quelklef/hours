module Hours.Time
  ( Instant
  , getNow
  , asMilliseconds
  , isToday
  , Minutes(..)
  , minutesBetween
  ) where

import Prelude

import Effect (Effect)
import Data.Int (round)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)

newtype Instant = Instant { millis :: Number }

derive instance Generic Instant _
instance DecodeJson Instant where decodeJson = genericDecodeJson
instance EncodeJson Instant where encodeJson = genericEncodeJson

foreign import getNow_f
  :: (Number -> Instant)
  -> Effect Instant

getNow :: Effect Instant
getNow = getNow_f \millis -> Instant { millis }

asMilliseconds :: Instant -> Number
asMilliseconds (Instant { millis }) = millis

foreign import isToday :: Instant -> Effect Boolean

newtype Minutes = Minutes Int

derive instance Generic Minutes _
instance DecodeJson Minutes where decodeJson = genericDecodeJson
instance EncodeJson Minutes where encodeJson = genericEncodeJson

instance Semigroup Minutes where
  append (Minutes n) (Minutes m) = Minutes $ m + n

instance Monoid Minutes where
  mempty = Minutes 0

minutesBetween :: Instant -> Instant -> Minutes
minutesBetween a b = Minutes <<< round $ (toMillis b - toMillis a) `div` (1000.0 * 60.0)
  where toMillis (Instant { millis }) = millis
