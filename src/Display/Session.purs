module Hours.Display.Session (Session(..), displaySession, displayAppSession) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Foldable (fold)
import Data.Array (filter, intercalate, catMaybes)
import Data.Array as Array
import Data.String.Common (split) as Str
import Data.String.Pattern (Pattern(..)) as Str

import Hours.Core (Event(..), EventPayload(..), Journal)
import Hours.Time (Instant)
import Hours.Simulate (App(..))
import Hours.Display.Util (indent, displayInstant_HHMM)

-- Contiguous subsequence of the app journal,
-- expected to begin with a session-start event and end with a session-end event
type Session = Journal

displayAppSession :: App -> Either String String
displayAppSession (App app) =
  case app.session of
    Nothing -> Left "No running session"
    Just sessInfo -> Right $ intercalate "\n"
      [ "Active session on topic " <> sessInfo.topicName
      , displaySession $ getSession sessInfo app.journal
      ]


getSession :: { started :: Instant, topicName :: String } -> Journal -> Session
getSession =

  \{ started, topicName } journal ->
      journal
      # filter (\(Event event) -> event.timestamp >= started)
      # filter (pertainsTo topicName)

  where

  pertainsTo :: String -> Event -> Boolean
  pertainsTo tName (Event event) = case event.payload of

    EventPayload_TopicNew     { topicName } -> topicName == tName
    EventPayload_TopicSetDesc { topicName } -> topicName == tName
    EventPayload_TopicRetire  { topicName } -> topicName == tName
    EventPayload_TopicFlush   { topicName } -> topicName == tName
    EventPayload_TopicLog     { topicName } -> topicName == tName

    -- v Known to pertain to the topic because, do to previous filtering,
    --   they all lie within the current session
    EventPayload_SessionStart _ -> true
    EventPayload_SessionStop    -> true
    EventPayload_SessionJot _   -> true


-- Display a session. Can be completed or active.
displaySession :: Session -> String
displaySession sess =

  sess
    # map displayEvent
    # catMaybes
    # intercalate "\n"
    # decorate

  where

  displayEvent :: Event -> Maybe String
  displayEvent (Event event) =
    let content = case event.payload of
          EventPayload_SessionStart _      -> Just "started session"
          EventPayload_SessionStop         -> Just "stopped session"
          EventPayload_SessionJot { note } -> Just $ "note: " <> note

          EventPayload_TopicNew _     -> Nothing
          EventPayload_TopicSetDesc _ -> Nothing
          EventPayload_TopicRetire _  -> Nothing
          EventPayload_TopicFlush _   -> Nothing
          EventPayload_TopicLog _     -> Nothing
    in
      content <#> \str -> displayInstant_HHMM event.timestamp <> ": " <> str

  decorate :: String -> String
  decorate =
    indent " "
    >>> Str.split (Str.Pattern "\n")
    >>> (\lines -> fold
                   [ ("┌" <> _) <$> Array.take 1 lines
                   , ("│" <> _) <$> (Array.drop 1 $ Array.dropEnd 1 $ lines)
                   , (end <> _) <$> Array.takeEnd 1 lines
                   ])
    >>> intercalate "\n"
    >>> indent " "


  end = case Array.takeEnd 1 sess of
    [Event { payload: EventPayload_SessionStop }] -> "└"
    _ -> "│"
