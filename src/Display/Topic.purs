module Hours.Display.Topic (displayTopic) where

import Prelude

import Data.Array (intercalate)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.Monoid (power)
import Data.String.CodeUnits (length)

import Hours.Simulate (Topic)
import Hours.Core (Event(..), EventPayload(..), Journal)
import Hours.Display.Util (displayPairs, displayMinutes, displayInstant_Date, displayInstant_HHMM, indent)

displayTopic :: Topic -> String
displayTopic topic =

  let

    infoStr = displayPairs
      [ "name" /\ topic.name
      , "description" /\ topic.desc
      , "time total" /\ displayMinutes topic.timeTotal
      , "time unbilled" /\ displayMinutes topic.timeUnbilled
      , "retired?" /\ show topic.isRetired
      ]

  in intercalate "\n"
    [ "Topic: " <> topic.name
    , ""
    , infoStr
    , ""
    , "Log:"
    , displayLog topic.journal
    ]

  where

  displayLog :: Journal -> String
  displayLog = go { curHeader: "" } # map (intercalate "\n" >>> indent " ")
    where
    go :: { curHeader :: String } -> Journal -> Array String
    go { curHeader } =
      Array.uncons >>> case _ of
        Nothing -> []
        Just { head: Event event, tail: events } ->
          let
            withDecor prefix str = prefix <> " " <> displayInstant_HHMM event.timestamp <> ": " <> str
            mLine = case event.payload of
              EventPayload_TopicNew     _ -> Nothing
              EventPayload_TopicSetDesc _ -> Nothing
              EventPayload_TopicRetire  _ -> Nothing
              EventPayload_TopicFlush   _ -> Nothing
              EventPayload_TopicLog     { amount } ->
                Just $ withDecor "•╴" $ "Logged " <> displayMinutes amount <> (if event.comment == "" then "" else ": " <> event.comment)
              EventPayload_SessionStart _ ->
                Just $ withDecor "┌╴" $ if event.comment == "" then "session started" else event.comment
              EventPayload_SessionStop ->
                Just $ withDecor "└╴" $ if event.comment == "" then "session stopped" else event.comment
              EventPayload_SessionJot { note } ->
                Just $ withDecor "│ " $ note <> (if event.comment == "" then "" else "\n" <> event.comment)
            header = let d = displayInstant_Date event.timestamp in "\n" <> d <> "\n" <> "╶" <> ("─" `power` (length d - 2)) <> "╴"
            lines =
              ( if header == curHeader then [] else [header] )
              <> ( case mLine of
                Nothing -> []
                Just line -> [line] )

          in lines <> go { curHeader: header } events
