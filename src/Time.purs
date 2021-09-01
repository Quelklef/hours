module Hours.Time
  ( Instant
  , getNow
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

import Hours.Pretty (class Pretty)

newtype Instant = Instant { millis :: Number }

derive instance Generic Instant _
instance DecodeJson Instant where decodeJson = genericDecodeJson
instance EncodeJson Instant where encodeJson = genericEncodeJson

foreign import getNow_f
  :: (Number -> Instant)
  -> Effect Instant

getNow :: Effect Instant
getNow = getNow_f \millis -> Instant { millis }

foreign import prettifyMillis :: Number -> String

instance Pretty Instant where
  pretty (Instant { millis }) = prettifyMillis millis

newtype Minutes = Minutes Int

derive instance Generic Minutes _
instance DecodeJson Minutes where decodeJson = genericDecodeJson
instance EncodeJson Minutes where encodeJson = genericEncodeJson

instance Semigroup Minutes where
  append (Minutes n) (Minutes m) = Minutes $ m + n

instance Monoid Minutes where
  mempty = Minutes 0

instance Pretty Minutes where
  pretty (Minutes n) =
    let hours = n `div` 60
        minutes = n `mod` 60
    in show hours <> "h " <> show minutes <> "m"

minutesBetween :: Instant -> Instant -> Minutes
minutesBetween a b = Minutes <<< round $ (toMillis b - toMillis a) `div` (1000.0 * 60.0)
  where toMillis (Instant { millis }) = millis
