module Hours.Simulate (simulate) where

import Prelude

import Data.Map as Map
import Data.Foldable (class Foldable, foldl)
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..))

import Hours.Types (Result, Event(..), EventPayload(..), Topic, App(..))
import Hours.Time (Minutes, minutesBetween)

simulate :: forall f. Foldable f => f Event -> Result App
simulate actions = foldl (\s a -> s >>= execute a) (pure app0) actions

app0 :: App
app0 = App
  { topics: Map.empty
  }

mkTopic0 :: String -> Topic
mkTopic0 name =
  { name
  , workedTotal: mempty
  , workedUnbilled: mempty
  , activeWork: Nothing
  , isRetired: false
  }

withTopic :: String -> (Topic -> Result Topic) -> App -> Result App
withTopic topicName f (App app) = do
  topic <- Map.lookup topicName app.topics # note "Topic not found"
  topic' <- f topic
  let topics' = Map.insert topicName topic' app.topics
  pure (App $ app { topics = topics' })

execute :: Event -> App -> Result App
execute (Event event) (App app) = case event.payload of

  EventPayload_NewTopic { topicName } ->
    if Map.member topicName app.topics
    then Left "Topic already exists"
    else Right (App $ app { topics = Map.insert topicName (mkTopic0 topicName) app.topics })

  EventPayload_RetireTopic { topicName } ->
    (App app) # withTopic topicName \topic -> pure $ topic { isRetired = true }

  EventPayload_LogWork { topicName, amount } ->
    (App app) # withTopic topicName
      \topic -> pure $ topic { workedTotal = topic.workedTotal <> amount
                             , workedUnbilled = topic.workedUnbilled <> amount
                             }

  EventPayload_WorkStart { topicName } ->
    (App app) # withTopic topicName \topic ->
      case topic.activeWork of
        Just _ -> Left "Already have active work for this topic"
        Nothing -> Right $ topic { activeWork = Just { started: event.timestamp } }

  EventPayload_WorkStop { topicName } ->
    (App app) # withTopic topicName \topic ->
      case topic.activeWork of
        Nothing -> Left "No active work for this topic"
        Just { started } ->
          let duration = minutesBetween started event.timestamp
          in Right $ topic { activeWork = Nothing
                           , workedTotal = topic.workedTotal <> duration
                           , workedUnbilled = topic.workedUnbilled <> duration
                           }

  EventPayload_Billed { topicName } ->
    (App app) # withTopic topicName \topic ->
      pure $ topic { workedUnbilled = (mempty :: Minutes) }
