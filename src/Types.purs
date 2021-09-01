module Hours.Types where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either)
import Data.Foldable (fold)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)

import Hours.Time (Instant, Minutes)
import Hours.Pretty (class Pretty, pretty)

type Result = Either String

type Journal = Array Event

data Event = Event
  { timestamp :: Instant
  , note :: Maybe String
  , payload :: EventPayload
  }

derive instance Generic Event _
instance DecodeJson Event where decodeJson = genericDecodeJson
instance EncodeJson Event where encodeJson = genericEncodeJson

instance Pretty Event where
  pretty (Event event) = fold
    [ pretty event.payload
    , "\n  time: " <> pretty event.timestamp
    , case event.note of
      Nothing -> ""
      Just note -> "\n  note: " <> note
    ]

data EventPayload
  = EventPayload_NewTopic { topic :: String }
  | EventPayload_RetireTopic { topic :: String }
  | EventPayload_LogWork { topic :: String, amount :: Minutes }
  | EventPayload_WorkStart { topic :: String }
  | EventPayload_WorkStop { topic :: String }
  | EventPayload_Billed { topic :: String }

derive instance Generic EventPayload _
instance DecodeJson EventPayload where decodeJson = genericDecodeJson
instance EncodeJson EventPayload where encodeJson = genericEncodeJson

instance Pretty EventPayload where
  pretty = case _ of
    EventPayload_NewTopic { topic } -> "Created topic " <> topic
    EventPayload_RetireTopic { topic } -> "Retired topic " <> topic
    EventPayload_LogWork { topic, amount } -> "Logged " <> pretty amount <> " on topic " <> topic
    EventPayload_WorkStart { topic } -> "Began work on " <> topic
    EventPayload_WorkStop { topic } -> "Finished work on " <> topic
    EventPayload_Billed { topic } -> "Billed " <> topic

type TopicState =
  { name :: String
  , workedTotal :: Minutes
  , workedUnbilled :: Minutes
  , activeWork :: Maybe { started :: Instant }
  , isRetired :: Boolean
  }
