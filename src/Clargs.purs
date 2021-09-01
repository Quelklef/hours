module Hours.Clargs (Clargs(..), Cmd(..), cli) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Foldable (fold)
import Options.Applicative.Types (Parser, ParserInfo) as O
import Options.Applicative.Extra (helper) as O
import Options.Applicative.Builder as OB

import Hours.Time (Instant, Minutes(..))
import Hours.Types (Event(..), EventPayload(..))

data Clargs = Clargs
  { journalLoc :: String
  , cmd :: Cmd
  }

data Cmd
  = Cmd_Status
  | Cmd_History
  | Cmd_Undo
  | Cmd_Append ({ now :: Instant } -> Event)

cli :: O.ParserInfo Clargs
cli = OB.info (O.helper <*> parser) desc
  where

  desc = OB.progDesc "CLI tool for tracking hours"

  parser = ado

    journalLoc <- OB.option OB.str $ fold
      [ OB.help "Journal file location"
      , OB.long "journal"
      , OB.short 'j'
      , OB.metavar "JOURNAL"
      , OB.value "./journal.txt"
      , OB.showDefault
      ]

    cmd <- OB.subparser $ fold
      [ cmd_status
      , cmd_history
      , cmd_undo
      , cmd_newTopic
      , cmd_retireTopic
      , cmd_logWork
      , cmd_startWork
      , cmd_stopWork
      , cmd_billed
      ]

    in Clargs { journalLoc, cmd }

cmd_status :: OB.Mod OB.CommandFields Cmd
cmd_status = OB.command "status" $ OB.info (O.helper <*> parser) desc
  where
  desc = OB.progDesc "Display current status"
  parser = pure Cmd_Status

cmd_history :: OB.Mod OB.CommandFields Cmd
cmd_history = OB.command "history" $ OB.info (O.helper <*> parser) desc
  where
  desc = OB.progDesc "Display event log"
  parser = pure Cmd_History

cmd_undo :: OB.Mod OB.CommandFields Cmd
cmd_undo = OB.command "undo" $ OB.info (O.helper <*> parser) desc
  where
  desc = OB.progDesc "Pop the most recent event"
  parser = pure Cmd_Undo

noteOpt :: O.Parser (Maybe String)
noteOpt = OB.option (Just <$> OB.str) $ fold
  [ OB.help "Note"
  , OB.long "note"
  , OB.metavar "NOTE"
  , OB.value Nothing
  , OB.showDefault
  ]

cmd_newTopic :: OB.Mod OB.CommandFields Cmd
cmd_newTopic = OB.command "new-topic" $ OB.info (O.helper <*> parser) desc
  where
  desc = OB.progDesc "Create a new topic"
  parser = ado
    topicName <- OB.option OB.str (OB.help "Topic name" <> OB.long "name")
    note <- noteOpt
    in Cmd_Append $ \{ now: timestamp } ->
      Event { timestamp, note, payload: EventPayload_NewTopic { topicName }}

cmd_retireTopic :: OB.Mod OB.CommandFields Cmd
cmd_retireTopic = OB.command "retire-topic" $ OB.info (O.helper <*> parser) desc
  where
  desc = OB.progDesc "Retire a topic"
  parser = ado
    topicName <- OB.option OB.str (OB.help "Topic name" <> OB.long "topic" <> OB.short 't')
    note <- noteOpt
    in Cmd_Append $ \{ now: timestamp } ->
      Event { timestamp, note, payload: EventPayload_RetireTopic { topicName }}

cmd_logWork :: OB.Mod OB.CommandFields Cmd
cmd_logWork = OB.command "log" $ OB.info (O.helper <*> parser) desc
  where
  desc = OB.progDesc "Log work on a topic"
  parser = ado
    topicName <- OB.option OB.str (OB.help "Topic name" <> OB.long "topic" <> OB.short 't')
    hours <- OB.option OB.int (OB.help "Hours worked" <> OB.short 'H' <> OB.long "hours")
    minutes <- OB.option OB.int (OB.help "Minutes worked" <> OB.short 'M' <> OB.long "minutes")
    let amount = Minutes $ hours * 60 + minutes
    note <- noteOpt
    in Cmd_Append $ \{ now: timestamp } ->
      Event { timestamp, note, payload: EventPayload_LogWork { topicName, amount }}

cmd_startWork :: OB.Mod OB.CommandFields Cmd
cmd_startWork = OB.command "start-work" $ OB.info (O.helper <*> parser) desc
  where
  desc = OB.progDesc "Start working on a topic"
  parser = ado
    topicName <- OB.option OB.str (OB.help "Topic name" <> OB.long "topic" <> OB.short 't')
    note <- noteOpt
    in Cmd_Append $ \{ now: timestamp } ->
      Event { timestamp, note, payload: EventPayload_WorkStart { topicName }}

cmd_stopWork :: OB.Mod OB.CommandFields Cmd
cmd_stopWork = OB.command "stop-work" $ OB.info (O.helper <*> parser) desc
  where
  desc = OB.progDesc "Stop working on a topic"
  parser = ado
    topicName <- OB.option OB.str (OB.help "Topic name" <> OB.long "topic" <> OB.short 't')
    note <- noteOpt
    in Cmd_Append $ \{ now: timestamp } ->
      Event { timestamp, note, payload: EventPayload_WorkStop { topicName }}

cmd_billed :: OB.Mod OB.CommandFields Cmd
cmd_billed = OB.command "billed" $ OB.info (O.helper <*> parser) desc
  where
  desc = OB.progDesc "Note that you billed a topic"
  parser = ado
    topicName <- OB.option OB.str (OB.help "Topic name" <> OB.long "topic" <> OB.short 't')
    note <- noteOpt
    in Cmd_Append $ \{ now: timestamp } ->
      Event { timestamp, note, payload: EventPayload_Billed { topicName }}
