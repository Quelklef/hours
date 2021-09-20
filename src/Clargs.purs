module Hours.Clargs (Clargs(..), Cmd(..), cli) where

import Prelude

import Data.Foldable (fold)
import Options.Applicative.Types (Parser, ParserInfo) as O
import Options.Applicative.Extra (helper) as O
import Options.Applicative.Builder as OB

import Hours.Time (Instant, parseMinutes, Minutes(..))
import Hours.Core (Event(..), EventPayload(..))

data Clargs = Clargs
  { journalLoc :: String
  , cmd :: Cmd
  }

data Cmd

  = Cmd_Display { todayOnly :: Boolean }
  | Cmd_DisplayTopic { topicName :: String, todayOnly :: Boolean }
  | Cmd_DisplaySession
  | Cmd_DisplayEventlog

  | Cmd_EventlogAppend ({ timestamp :: Instant } -> Event)
  | Cmd_EventlogPop
  | Cmd_EventlogEdit

type CommandParser = OB.Mod OB.CommandFields Cmd

cli :: O.ParserInfo Clargs
cli = OB.info (O.helper <*> parser) desc
  where

  desc = OB.progDesc "CLI tool for tracking hours"

  parser = ado

    journalLoc <- OB.option OB.str $ fold
      [ OB.help "Journal file location"
      , OB.long "journal"
      , OB.short 'j'
      , OB.metavar "LOC"
      , OB.value "./journal.json"
      , OB.showDefault
      ]

    cmd <- OB.subparser $ fold
      [ cmd_show
      , cmd_topic
      , cmd_session
      , cmd_eventlog
      ]

    in Clargs { journalLoc, cmd }


cmd_show :: CommandParser
cmd_show = OB.command "show" $ OB.info (O.helper <*> parser) desc
  where
  desc = OB.progDesc "Display program status"
  parser = ado
    todayOnly <- OB.switch (OB.help "only consider work done today" <> OB.long "today-only" <> OB.short 'd')
    in Cmd_Display { todayOnly }


cmd_topic :: CommandParser
cmd_topic = OB.command "topic" $ OB.info (O.helper <*> parser) desc
  where
  desc = OB.progDesc "Topic (i.e., project) -related commands"
  parser = OB.subparser $ fold
    [ cmd_topicShow
    , cmd_topicNew
    , cmd_topicSetDesc
    , cmd_topicRetire
    , cmd_topicFlush
    , cmd_topicLog
    ]

cmd_topicShow :: CommandParser
cmd_topicShow = OB.command "show" $ OB.info (O.helper <*> parser) desc
  where
  desc = OB.progDesc "Show topic status"
  parser = ado
    topicName <- topicNameOpt
    todayOnly <- OB.switch (OB.help "only consider work done today" <> OB.long "today-only" <> OB.short 'd')
    in Cmd_DisplayTopic { topicName, todayOnly }

cmd_topicNew :: CommandParser
cmd_topicNew = OB.command "new" $ OB.info (O.helper <*> parser) desc
  where
  desc = OB.progDesc "Create new topic"
  parser = eventOf ado
    topicName <- OB.option OB.str (OB.help "topic name" <> OB.long "name")
    in EventPayload_TopicNew { topicName }

cmd_topicSetDesc :: CommandParser
cmd_topicSetDesc = OB.command "set-desc" $ OB.info (O.helper <*> parser) desc
  where
  desc = OB.progDesc "Set topic description"
  parser = eventOf ado
    topicName <- topicNameOpt
    desc <- OB.option OB.str (OB.help "new description" <> OB.long "desc")
    in EventPayload_TopicSetDesc { topicName, desc }

cmd_topicRetire :: CommandParser
cmd_topicRetire = OB.command "retire" $ OB.info (O.helper <*> parser) desc
  where
  desc = OB.progDesc "Retire a topic. This is a soft-delete"
  parser = eventOf ado
    topicName <- topicNameOpt
    in EventPayload_TopicRetire { topicName }

cmd_topicFlush :: CommandParser
cmd_topicFlush = OB.command "flush" $ OB.info (O.helper <*> parser) desc
  where
  desc = OB.progDesc "Reset billable hours to zero"
  parser = eventOf ado
    topicName <- topicNameOpt
    retainAmt <- OB.option (OB.maybeReader parseMinutes) $ fold
      [ OB.long "retain"
      , OB.help "amount to NOT flush"
      , OB.value $ Minutes 0
      ]
    in EventPayload_TopicFlush { topicName, retain: retainAmt }

cmd_topicLog :: CommandParser
cmd_topicLog = OB.command "log" $ OB.info (O.helper <*> parser) desc
  where
  desc = OB.progDesc "Record work"
  parser = eventOf ado
    topicName <- topicNameOpt
    amount <- OB.option (OB.maybeReader parseMinutes) (OB.help "amount of time" <> OB.long "amount" <> OB.short 'a')
    in EventPayload_TopicLog { topicName, amount }


cmd_session :: CommandParser
cmd_session = OB.command "session" $ OB.info (O.helper <*> parser) desc
  where
  desc = OB.progDesc "Start and manage a time-tracking session"
  parser = OB.subparser $ fold
    [ cmd_sessionShow
    , cmd_sessionStart
    , cmd_sessionStop
    , cmd_sessionJot
    ]

cmd_sessionShow :: CommandParser
cmd_sessionShow = OB.command "show" $ OB.info (O.helper <*> parser) desc
  where
  desc = OB.progDesc "Show session status"
  parser = pure Cmd_DisplaySession

cmd_sessionStart :: CommandParser
cmd_sessionStart = OB.command "start" $ OB.info (O.helper <*> parser) desc
  where
  desc = OB.progDesc "Start a timer towards a topic"
  parser = eventOf ado
    topicName <- topicNameOpt
    in EventPayload_SessionStart { topicName }

cmd_sessionStop :: CommandParser
cmd_sessionStop = OB.command "stop" $ OB.info (O.helper <*> parser) desc
  where
  desc = OB.progDesc "Stop the timer and add the timed hours to the sessions's topic"
  parser = eventOf (pure EventPayload_SessionStop)

cmd_sessionJot :: CommandParser
cmd_sessionJot = OB.command "jot" $ OB.info (O.helper <*> parser) desc
  where
  desc = OB.progDesc "Record a note"
  parser = eventOf ado
    note <- OB.option OB.str (OB.help "text to note down" <> OB.long "note" <> OB.short 'n')
    in EventPayload_SessionJot { note }


cmd_eventlog :: CommandParser
cmd_eventlog = OB.command "eventlog" $ OB.info (O.helper <*> parser) desc
  where
  desc = OB.progDesc "Eventlog-related commands"
  parser = OB.subparser $ fold
    [ cmd_eventlogShow
    , cmd_eventlogUndo
    , cmd_eventlogEdit
    ]

cmd_eventlogShow :: CommandParser
cmd_eventlogShow = OB.command "show" $ OB.info (O.helper <*> parser) desc
  where
  desc = OB.progDesc "Show the event log"
  parser = pure Cmd_DisplayEventlog

cmd_eventlogUndo :: CommandParser
cmd_eventlogUndo = OB.command "undo" $ OB.info (O.helper <*> parser) desc
  where
  desc = OB.progDesc "Popt the most recent event"
  parser = pure Cmd_EventlogPop

cmd_eventlogEdit :: CommandParser
cmd_eventlogEdit = OB.command "edit" $ OB.info (O.helper <*> parser) desc
  where
  desc = OB.progDesc "Edit the event log in $VISUAL"
  parser = pure Cmd_EventlogEdit


topicNameOpt :: O.Parser String
topicNameOpt = OB.option OB.str $ fold
  [ OB.help "Topic name"
  , OB.long "topic"
  , OB.short 't'
  , OB.metavar "<topic>"
  ]

eventOf :: O.Parser EventPayload -> O.Parser Cmd
eventOf payloadOpt = ado
  payload <- payloadOpt
  comment <- OB.option OB.str $ fold
    [ OB.help "Event comment"
    , OB.long "comment"
    , OB.short 'c'
    , OB.metavar "<comment>"
    , OB.value ""
    ]
  in Cmd_EventlogAppend \{ timestamp } -> Event { timestamp, comment, payload }
