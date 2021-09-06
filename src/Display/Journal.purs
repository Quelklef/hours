module Hours.Display.Journal (displayJournal) where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Data.Array (intercalate)
import Data.Foldable (fold)
import Data.Monoid (guard)

import Hours.Display.Util (displayMinutes, displayInstant_DateTime, displayPairs)
import Hours.Core (Journal, Event(..), EventPayload(..))

displayJournal :: Journal -> String
displayJournal = map displayEvent >>> intercalate "\n\n"

displayEvent :: Event -> String
displayEvent = displayPairs <<< eventToPairs

eventToPairs :: Event -> Array (String /\ String)
eventToPairs (Event event) = fold
  [ [ "time" /\ displayInstant_DateTime event.timestamp ]
  , payloadToPairs event.payload
  , guard (event.comment /= "") $ [ "comment" /\ event.comment ]
  ]

payloadToPairs :: EventPayload -> Array (String /\ String)
payloadToPairs = case _ of
  EventPayload_TopicNew { topicName } ->
    [ "action" /\ "create new topic"
    , "topic name" /\ topicName
    ]

  EventPayload_TopicSetDesc { topicName, desc } ->
    [ "action" /\ "set topic description"
    , "on topic" /\ topicName
    , "desc" /\ desc
    ]

  EventPayload_TopicRetire { topicName } ->
    [ "action" /\ "retire topic"
    , "topic" /\ topicName
    ]

  EventPayload_TopicFlush { topicName } ->
    [ "action" /\ "flush topic billable hours"
    , "on topic" /\ topicName
    ]

  EventPayload_TopicLog { topicName, amount } ->
    [ "action" /\ "log hours"
    , "on topic" /\ topicName
    , "amount" /\ displayMinutes amount
    ]

  EventPayload_SessionStart { topicName } ->
    [ "action" /\ "start session"
    , "on topic" /\ topicName
    ]

  EventPayload_SessionStop ->
    [ "action" /\ "stop session"
    ]

  EventPayload_SessionJot { note } ->
    [ "action" /\ "jot note down in session"
    , "note" /\ note
    ]

