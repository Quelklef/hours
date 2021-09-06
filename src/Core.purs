module Hours.Core where

import Prelude

import Data.Either (Either(..))
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Decode ((.:), JsonDecodeError(MissingValue)) as A

import Hours.Time (Instant, Minutes)

type Journal = Array Event

data Event = Event
  { timestamp :: Instant
  , comment :: String
  , payload :: EventPayload
  }

data EventPayload

  = EventPayload_TopicNew { topicName :: String }
  | EventPayload_TopicSetDesc { topicName :: String, desc :: String }
  | EventPayload_TopicRetire { topicName :: String }
  | EventPayload_TopicFlush { topicName :: String }
  | EventPayload_TopicLog { topicName :: String, amount :: Minutes }

  | EventPayload_SessionStart { topicName :: String }
  | EventPayload_SessionStop
  | EventPayload_SessionJot { note :: String }


-- aeson instances --

instance EncodeJson Event where
  encodeJson (Event event) = encodeJson event

instance DecodeJson Event where
  decodeJson = decodeJson >>> map Event

instance EncodeJson EventPayload where
  encodeJson = case _ of

    EventPayload_TopicNew { topicName } ->
      encodeJson { variant: "TopicNew", topicName }

    EventPayload_TopicSetDesc { topicName, desc } ->
      encodeJson { variant: "TopicSetDesc", topicName, desc }

    EventPayload_TopicRetire { topicName } ->
      encodeJson { variant: "TopicRetire", topicName }

    EventPayload_TopicFlush { topicName } ->
      encodeJson { variant: "TopicFlush", topicName }

    EventPayload_TopicLog { topicName, amount } ->
      encodeJson { variant: "TopicLog", topicName, amount }

    EventPayload_SessionStart { topicName } ->
      encodeJson { variant: "SessionStart", topicName }

    EventPayload_SessionStop ->
      encodeJson { variant: "SessionStop" }

    EventPayload_SessionJot { note } ->
      encodeJson { variant: "SessionJot", note }

instance DecodeJson EventPayload where
  decodeJson json = do
    tag <- (_ A..: "variant") =<< decodeJson json
    case tag of
      "TopicNew"     -> EventPayload_TopicNew     <$> decodeJson json
      "TopicSetDesc" -> EventPayload_TopicSetDesc <$> decodeJson json
      "TopicRetire"  -> EventPayload_TopicRetire  <$> decodeJson json
      "TopicFlush"   -> EventPayload_TopicFlush   <$> decodeJson json
      "TopicLog"     -> EventPayload_TopicLog     <$> decodeJson json
      "SessionStart" -> EventPayload_SessionStart <$> decodeJson json
      "SessionStop"  -> EventPayload_SessionStop   #  pure
      "SessionJot"   -> EventPayload_SessionJot   <$> decodeJson json
      _ -> Left A.MissingValue
