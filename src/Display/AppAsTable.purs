module Hours.Display.AppAsTable (displayAppAsTable) where

import Prelude

import Effect (Effect)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Map as Map
import Data.Foldable (length, fold)
import Data.String.CodeUnits (length) as Str
import Data.Monoid (power, guard)
import Data.Array (zip, intercalate, fromFoldable, catMaybes)
import Data.Array as Array
import Data.Tuple.Nested ((/\))

import Hours.Time (getNow, minutesBetween)
import Hours.Simulate (App(..))
import Hours.Display.Util (displayMinutes, indent, foldMax, surroundWith, justifyRight)

displayAppAsTable :: App -> Effect String
displayAppAsTable (App app) = do
  if Map.isEmpty app.topics
  then pure "Nothing to show!"
  else do
    now <- getNow

    let tableStr =
          displayTable ["Topic", "Logged", "Unbilled"] $
            app.topics
            # Map.values
            # fromFoldable
            # Array.filter (\topic -> not topic.isRetired)
            # map (\topic ->
              [ topic.name
                <> guard ((app.session <#> _.topicName) == Just topic.name) "*"
              , displayMinutes topic.timeTotal
              , displayMinutes topic.timeUnbilled
              ])

    let sessionStr =
          app.session
          # map \session -> fold
              [ "* Active session on topic "
              , session.topicName
              ," (for "
              , displayMinutes $ minutesBetween session.started now
              , ")"
              ]

    let fullStr =
          [ Just  tableStr
          , sessionStr
          ]
          # catMaybes
          # intercalate "\n\n"
          # indent "  "
          # surroundWith "\n"

    pure fullStr


-- Render an array of rows
displayTable :: Array String -> Array (Array String) -> String
displayTable headers rows =
  let
    horizMargin = 2
    marginStr = " " `power` horizMargin

    cellWidths =
      let hrows = Array.cons headers rows in
      Array.range 0 (foldMax length rows - 1)
      # map \colI -> hrows # foldMax \hrow -> Array.index hrow colI # map Str.length # fromMaybe 0

    format row =
      zip cellWidths row
      # map (\(width /\ cell) -> marginStr <> justifyRight width cell <> marginStr)
      # intercalate "│"

    divider = cellWidths
            # map (\width -> "─" `power` (width + 2 * horizMargin))
            # intercalate "┼"

  in ([format headers, divider] <> map format rows) # intercalate "\n"

