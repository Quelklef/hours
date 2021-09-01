module Hours.Pretty (class Pretty, pretty) where

import Prelude

import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Data.Map as Map
import Data.Foldable (class Foldable, fold, foldMap, length)
import Data.String.CodeUnits (length) as Str
import Data.Ord.Max (Max(..))
import Data.Monoid (power)
import Data.Array (zip, intercalate, fromFoldable)
import Data.Array as Array
import Data.Newtype (un)
import Data.Tuple.Nested ((/\))

import Hours.Time (Instant, asMilliseconds, Minutes(..))
import Hours.Types (Event(..), EventPayload(..), AppState(..))

class Pretty a where
  pretty :: a -> String

instance Pretty Instant where
  pretty = asMilliseconds >>> prettifyMillis

foreign import prettifyMillis :: Number -> String

instance Pretty Minutes where
  pretty (Minutes n) =
    let hours = n `div` 60
        minutes = n `mod` 60
    in show hours <> "h " <> show minutes <> "m"

instance Pretty Event where
  pretty (Event event) = fold
    [ pretty event.payload
    , "\n  time: " <> pretty event.timestamp
    , case event.note of
      Nothing -> ""
      Just note -> "\n  note: " <> note
    ]

instance Pretty EventPayload where
  pretty = case _ of
    EventPayload_NewTopic { topic } -> "Created topic " <> topic
    EventPayload_RetireTopic { topic } -> "Retired topic " <> topic
    EventPayload_LogWork { topic, amount } -> "Logged " <> pretty amount <> " on topic " <> topic
    EventPayload_WorkStart { topic } -> "Began work on " <> topic
    EventPayload_WorkStop { topic } -> "Finished work on " <> topic
    EventPayload_Billed { topic } -> "Billed " <> topic

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
