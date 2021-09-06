module Hours.Simulate (App(..), Topic, simulate) where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Foldable (foldl)
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..))

import Hours.Core (Event(..), EventPayload(..), Journal)
import Hours.Time (Instant, Minutes, minutesBetween)

data App = App
  { journal :: Journal  -- technically the only necessary info
  , topics :: Map String Topic
  , session :: Maybe { started :: Instant, topicName :: String }
  }

type Topic =
  { journal :: Journal  -- filtered version of app journal
  , name :: String
  , desc :: String
  , timeTotal :: Minutes
  , timeUnbilled :: Minutes
  , isRetired :: Boolean
  }

simulate :: Array Event -> Either String App
simulate events = foldl (\s a -> s >>= execute a) (pure app0) events
  where
  app0 = App { journal: events, topics: Map.empty, session: Nothing }

mkTopic0 :: String -> Topic
mkTopic0 name =
  { journal: []
  , name: name
  , desc: ""
  , timeTotal: mempty
  , timeUnbilled: mempty
  , isRetired: false
  }

execute :: Event -> App -> Either String App
execute (Event event) = case event.payload of

  EventPayload_TopicNew { topicName } ->
    (\(App app) ->
      case Map.lookup topicName app.topics of
        Just _ -> Left $ "Topic '" <> topicName <> "' already exists"
        Nothing -> Right $ App $ app { topics = Map.insert topicName (mkTopic0 topicName) app.topics })
    >=> withTopic topicName (pure <<< appendEvent (Event event))

  EventPayload_TopicRetire { topicName } ->
    withTopic topicName $
      (\topic -> topic { isRetired = true })
      >>> appendEvent (Event event)
      >>> pure

  EventPayload_TopicSetDesc { topicName, desc } ->
    withTopic topicName $
      (\topic -> topic { desc = desc })
      >>> appendEvent (Event event)
      >>> pure

  EventPayload_TopicFlush { topicName } ->
    withTopic topicName $
      (\topic -> topic { timeUnbilled = (mempty :: Minutes) })
      >>> appendEvent (Event event)
      >>> pure

  EventPayload_TopicLog { topicName, amount } ->
    withTopic topicName $
      appendEvent (Event event)
      >>> (\topic -> topic { timeTotal = topic.timeTotal <> amount
                           , timeUnbilled = topic.timeUnbilled <> amount
                           })
      >>> pure

  EventPayload_SessionStart { topicName } ->
    (\(App app) -> case app.session of
      Just _ -> Left "Already have an active session"
      Nothing -> Right $ App $ app { session = Just { topicName, started: event.timestamp } })
    >=> (withTopic topicName (appendEvent (Event event) >>> pure))

  EventPayload_SessionStop ->
    \(App app) -> case app.session of
      Nothing -> Left "No active session"
      Just session ->
        let duration = minutesBetween session.started event.timestamp in
        (App $ app { session = Nothing })
          # withTopic session.topicName (
            (\topic -> topic { timeTotal = topic.timeTotal <> duration
                            , timeUnbilled = topic.timeUnbilled <> duration
                            })
            >>> appendEvent (Event event)
            >>> pure
          )

  EventPayload_SessionJot _ ->
    \(App app) -> case app.session of
      Nothing -> Left "No active session"
      Just { topicName } -> withTopic topicName (appendEvent (Event event) >>> pure) (App app)

withTopic :: String -> (Topic -> Either String Topic) -> App -> Either String App
withTopic topicName f (App app) = do
  topic <- Map.lookup topicName app.topics # note "Topic not found"
  topic' <- f topic
  Right $ App $ app { topics = Map.insert topicName topic' app.topics }

appendEvent :: Event -> Topic -> Topic
appendEvent event topic = topic { journal = topic.journal <> [event] }
