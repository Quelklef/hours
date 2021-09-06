module Hours.Display.Topic (displayTopic) where

import Prelude

import Data.Tuple.Nested ((/\))

import Data.Array (intercalate, head)
import Data.Array as Array
import Data.Maybe (Maybe(..))

import Hours.Simulate (Topic)
import Hours.Core (Event(..), EventPayload(..), Journal)
import Hours.Display.Util (displayPairs, displayMinutes, displayInstant_Date)
import Hours.Display.Session (Session, displaySession)

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

    sessionStrs =
      topic.journal
      # getSessions
      # map (\sess -> intercalate "\n"
                      [ case head sess of
                          Nothing -> "<err>"
                          Just (Event event) -> displayInstant_Date event.timestamp <> ":"
                      , displaySession sess
                      ])

  in intercalate "\n"
    [ "Topic: " <> topic.name
    , ""
    , infoStr
    , ""
    , "Complete sessions:"
    , ""
    , intercalate "\n\n" sessionStrs
    ]

  where

  getSessions :: Journal -> Array Session
  getSessions = blocks isSessionStart isSessionStop

  isSessionStart :: Event -> Boolean
  isSessionStart (Event event) = case event.payload of
    EventPayload_SessionStart _ -> true
    _ -> false

  isSessionStop :: Event -> Boolean
  isSessionStop (Event event) = case event.payload of
    EventPayload_SessionStop -> true
    _ -> false

  blocks :: forall a. (a -> Boolean) -> (a -> Boolean) -> Array a -> Array (Array a)
  blocks isStart isStop = go Nothing
    where
    go :: Maybe (Array a) -> Array a -> Array (Array a)
    go mCurBlock items =
      case Array.uncons items of
        Nothing -> []
        Just { head, tail } -> case mCurBlock of
          Nothing ->
            if isStart head
            then go (Just [head]) tail
            else go Nothing tail
          Just curBlock ->
            if isStop head
            then Array.cons (curBlock <> [head]) $ go Nothing tail
            else go (Just $ curBlock <> [head]) tail
