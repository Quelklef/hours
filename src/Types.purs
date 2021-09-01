module Hours.Types where

import Prelude

import Data.Maybe (Maybe)
import Data.Either (Either)
import Data.Map (Map)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)

import Hours.Time (Instant, Minutes)

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

data EventPayload
  = EventPayload_NewTopic { topicName :: String }
  | EventPayload_RetireTopic { topicName :: String }
  | EventPayload_LogWork { topicName :: String, amount :: Minutes }
  | EventPayload_WorkStart { topicName :: String }
  | EventPayload_WorkStop { topicName :: String }
  | EventPayload_Billed { topicName :: String }

derive instance Generic EventPayload _
instance DecodeJson EventPayload where decodeJson = genericDecodeJson
instance EncodeJson EventPayload where encodeJson = genericEncodeJson

type Topic =
  { name :: String
  , workedTotal :: Minutes
  , workedUnbilled :: Minutes
  , activeWork :: Maybe { started :: Instant }
  , isRetired :: Boolean
  }

newtype App = App
  { topics :: Map String Topic
  }
