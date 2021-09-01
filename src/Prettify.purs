module Hours.Prettify (prettifyApp, prettifyEvent) where

import Prelude

import Effect (Effect)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Map as Map
import Data.Foldable (class Foldable, fold, foldMap, length)
import Data.String.CodeUnits (length) as Str
import Data.Ord.Max (Max(..))
import Data.Monoid (power)
import Data.Array (zip, intercalate, fromFoldable)
import Data.Array as Array
import Data.Newtype (un)
import Data.Tuple.Nested ((/\))

import Hours.Time (Instant, asMilliseconds, Minutes(..), getNow, minutesBetween)
import Hours.Types (Event(..), EventPayload(..), App(..))

prettifyInstant :: Instant -> String
prettifyInstant = asMilliseconds >>> prettifyMillis

foreign import prettifyMillis :: Number -> String

prettifyMinutes :: Minutes -> String
prettifyMinutes (Minutes n) =
  let hours = n `div` 60
      minutes = n `mod` 60
  in show hours <> "h " <> show minutes <> "m"

prettifyEvent :: Event -> String
prettifyEvent (Event event) = fold
  [ prettifyEventPayload event.payload
  , "\n  time: " <> prettifyInstant event.timestamp
  , case event.note of
    Nothing -> ""
    Just note -> "\n  note: " <> note
  ]

  where

  prettifyEventPayload :: EventPayload -> String
  prettifyEventPayload = case _ of
    EventPayload_NewTopic { topicName } -> "Created topic " <> topicName
    EventPayload_RetireTopic { topicName } -> "Retired topic " <> topicName
    EventPayload_LogWork { topicName, amount } -> "Logged " <> prettifyMinutes amount <> " on topic " <> topicName
    EventPayload_WorkStart { topicName } -> "Began work on " <> topicName
    EventPayload_WorkStop { topicName } -> "Finished work on " <> topicName
    EventPayload_Billed { topicName } -> "Billed " <> topicName

prettifyApp :: App -> Effect String
prettifyApp (App app) = do
  if Map.isEmpty app.topics
  then pure "Nothing to see here"
  else do
    now <- getNow
    pure $ renderBox ["Topic", "Current", "Logged", "Unbilled"] $
      app.topics
      # Map.values
      # fromFoldable
      # Array.filter (\topic -> not topic.isRetired)
      # map (\topic ->
        [ topic.name
        , case topic.activeWork of
            Nothing -> ""
            Just { started } -> prettifyMinutes $ minutesBetween started now
        , prettifyMinutes topic.workedTotal
        , prettifyMinutes topic.workedUnbilled
        ])

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
