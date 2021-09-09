module Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Effect.Aff (launchAff_)
import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe(..))
import Data.Array as Array
import Data.Either (Either(..), note)
import Data.Bifunctor (lmap)
import Data.Map as Map
import Data.Traversable (traverse)
import Node.Process (exit)
import Node.Buffer (toString, fromString) as Buff
import Node.Encoding (Encoding(..)) as Buff
import Node.FS.Sync (readFile, writeFile, exists) as FS
import Options.Applicative.Extra (execParser)
import Data.Argonaut.Parser (jsonParser) as A
import Data.Argonaut.Core (stringify) as A
import Data.Argonaut.Decode (decodeJson) as A
import Data.Argonaut.Decode.Error (printJsonDecodeError) as A
import Data.Argonaut.Encode (encodeJson) as A

import Hours.Clargs (cli, Clargs(..), Cmd(..))
import Hours.Core (Journal, Event(..), EventPayload(..))
import Hours.Time (getNow, isToday)
import Hours.Simulate (simulate, App(..))
import Hours.Display.AppAsTable (displayAppAsTable)
import Hours.Display.Journal (displayJournal)
import Hours.Display.Topic (displayTopic)
import Hours.Display.Session (displayAppSession)

foreign import invokeEditor :: String -> Effect (Promise Unit)
foreign import prettifyJSON :: String -> Effect String

main :: Effect Unit
main = do

  Clargs { journalLoc, cmd } <- execParser cli

  case cmd of

    Cmd_Display { todayOnly } -> do
      journal <- readJournal journalLoc
      journal' <- if todayOnly then filterOnlyTodaysWork journal else pure journal
      app <- simulate journal' # throwLeft { while: "simulating" }
      log =<< displayAppAsTable app

    Cmd_DisplayTopic { topicName, todayOnly } -> do
      journal <- readJournal journalLoc
      journal' <- if todayOnly then filterOnlyTodaysWork journal else pure journal
      (App app) <- simulate journal' # throwLeft { while: "simulating" }
      topic <- Map.lookup topicName app.topics # note "No such topic" # failLeft
      log $ displayTopic topic

    Cmd_DisplayEventlog -> do
      journal <- readJournal journalLoc
      log $ displayJournal journal

    Cmd_DisplaySession -> do
      journal <- readJournal journalLoc
      app <- simulate journal # throwLeft { while: "simulating" }
      log =<< (displayAppSession app # failLeft)

    Cmd_EventlogAppend mkEvent -> do
      journal <- readJournal journalLoc
      now <- getNow
      let event = mkEvent { timestamp: now }
      let journal' = Array.snoc journal event
      _ <- simulate journal' # failLeft
      writeJournal journalLoc journal'

    Cmd_EventlogPop -> do
      journal <- readJournal journalLoc
      writeJournal journalLoc (Array.dropEnd 1 journal)

    Cmd_EventlogEdit -> do
      launchAff_ $ toAffE $ invokeEditor journalLoc

  where

  filterOnlyTodaysWork :: Journal -> Effect Journal
  filterOnlyTodaysWork = filterM shouldKeep
    where shouldKeep (Event event) = case event.payload of
            EventPayload_TopicNew _ -> pure true
            EventPayload_TopicSetDesc _ -> pure true
            EventPayload_TopicRetire _ -> pure true
            EventPayload_TopicFlush _ -> pure true
            EventPayload_TopicLog _ -> isToday event.timestamp
            EventPayload_SessionStart _ -> isToday event.timestamp
            EventPayload_SessionStop -> isToday event.timestamp
            EventPayload_SessionJot _ -> isToday event.timestamp
              -- ^ Note: this filtering could cause the journal to have 'session stop'
              --         and 'session jot' events without a corresponding 'session start'
              --         event, but that shouldn't be a problem.

  filterM :: forall m a. Monad m => (a -> m Boolean) -> Array a -> m (Array a)
  filterM p =
    traverse (\x -> do
      b <- p x
      pure $ if b then Just x else Nothing)
    >>> map Array.catMaybes

  readFile :: String -> Effect (Maybe String)
  readFile loc = do
    exists <- FS.exists loc
    if not exists then
      pure Nothing
    else do
      buff <- FS.readFile loc
      text <- Buff.toString Buff.UTF8 buff
      pure $ Just text

  writeFile :: String -> String -> Effect Unit
  writeFile loc text = do
    buff <- Buff.fromString text Buff.UTF8
    FS.writeFile loc buff

  readJournal :: String -> Effect Journal
  readJournal loc = do
    mText <- readFile loc
    case mText of
      Nothing -> pure []
      Just text -> do
        json <- A.jsonParser text # throwLeft { while: "parsing json" }
        journal <- A.decodeJson json # lmap A.printJsonDecodeError # throwLeft { while: "parsing journal" }
        pure journal

  writeJournal :: String -> Journal -> Effect Unit
  writeJournal loc journal = do
    let jsonUgly = A.stringify $ A.encodeJson journal
    jsonPretty <- prettifyJSON jsonUgly
    writeFile loc jsonPretty

  throwLeft :: forall x a. Show x => { while :: String } -> Either x a -> Effect a
  throwLeft { while } = case _ of
    Left err -> throw ("Error while " <> while <> ": " <> show err)
    Right val -> pure val

  failLeft :: forall a. Either String a -> Effect a
  failLeft = case _ of
    Left err -> do
      log err
      exit 1
    Right val -> pure val
