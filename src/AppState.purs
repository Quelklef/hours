module Hours.AppState (AppState(..)) where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..))
import Type.Proxy (Proxy(..))

import Hours.Types (Result, TopicState, Event(..), EventPayload(..))
import Hours.Time (Minutes, minutesBetween)
import Hours.Gar (class Gar)

newtype AppState = AppState (Map String TopicState)

mkTopicState :: String -> TopicState
mkTopicState name =
  { name
  , workedTotal: mempty
  , workedUnbilled: mempty
  , activeWork: Nothing
  , isRetired: false
  }

withTopic :: String -> (TopicState -> Result TopicState) -> AppState -> Result AppState
withTopic topic f (AppState states) = do
  state <- Map.lookup topic states # note "Topic not found"
  state' <- f state
  let states' = Map.insert topic state' states
  pure (AppState states')

instance Gar Result Event AppState where
  initial Proxy Proxy = AppState Map.empty

  execute (Event event) appState@(AppState states) = case event.payload of

    EventPayload_NewTopic { topic } ->
      if Map.member topic states
      then Left "Topic already exists"
      else Right $ AppState $ Map.insert topic (mkTopicState topic) states

    EventPayload_RetireTopic { topic } ->
      appState # withTopic topic (\s -> pure $ s { isRetired = true })

    EventPayload_LogWork { topic, amount } ->
      appState # withTopic topic
        \state -> pure $ state { workedTotal = state.workedTotal <> amount
                               , workedUnbilled = state.workedUnbilled <> amount
                               }

    EventPayload_WorkStart { topic } ->
      appState # withTopic topic \state ->
        case state.activeWork of
          Just _ -> Left "Already have active work for this topic"
          Nothing -> Right $ state { activeWork = Just { started: event.timestamp } }

    EventPayload_WorkStop { topic } ->
      appState # withTopic topic \state ->
        case state.activeWork of
          Nothing -> Left "No active work for this topic"
          Just { started } ->
            let duration = minutesBetween started event.timestamp
            in Right $ state { activeWork = Nothing
                             , workedTotal = state.workedTotal <> duration
                             , workedUnbilled = state.workedUnbilled <> duration
                             }

    EventPayload_Billed { topic } ->
      appState # withTopic topic \state ->
        pure $ state { workedUnbilled = (mempty :: Minutes) }
