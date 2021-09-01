module Hours.AppState (AppState(..)) where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Data.Foldable (class Foldable, foldMap, length)
import Data.String.CodeUnits (length) as Str
import Data.Ord.Max (Max(..))
import Data.Newtype (un)
import Data.Tuple.Nested ((/\))
import Data.Monoid (power)
import Data.Array (zip, intercalate, fromFoldable)
import Data.Array as Array
import Type.Proxy (Proxy(..))

import Hours.Types (Result, TopicState, Event(..), EventPayload(..))
import Hours.Time (Minutes, minutesBetween)
import Hours.Gar (class Gar)
import Hours.Pretty (class Pretty, pretty)

newtype AppState = AppState (Map String TopicState)

instance Pretty AppState where
  pretty (AppState states) =
    if Map.isEmpty states
    then "Nothing to see here"
    else renderBox ["Topic", "Total", "Unbilled"] $
      states
      # Map.values
      # map (\state ->
        [ state.name <> (if isJust state.activeWork then "*" else " ")
        , pretty state.workedTotal
        , pretty state.workedUnbilled
        ])
      # fromFoldable

-- Render an array of rows
renderBox :: Array String -> Array (Array String) -> String
renderBox headers rows =
  let
    padTo :: Int -> String -> String
    padTo width str = str <> " " `power` (width - Str.length str)

    foldMax :: forall f k a. Foldable f => Bounded k => Ord k => (a -> k) -> f a -> k
    foldMax f l = l # foldMap (Max <<< f) # un Max

    horizMargin = 2

    cellWidths =
      let hrows = Array.cons headers rows in
      Array.range 0 (foldMax length rows - 1)
      # map \colI -> hrows # foldMax \hrow -> Array.index hrow colI # map Str.length # fromMaybe 0

    format row =
      zip cellWidths row
      # map (\(width /\ cell) -> (" " `power` horizMargin) <> padTo (width + horizMargin) cell)
      # intercalate "│"

    divider = cellWidths
            # map (\width -> "─" `power` (width + 2 * horizMargin))
            # intercalate "┼"

  in ([format headers, divider] <> map format rows) # intercalate "\n"

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
