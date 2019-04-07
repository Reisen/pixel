module Commands.RunProjections
  ( commandRunProjections
  , runProjections
  ) where

import Protolude
import Commands.Types           ( Options (..), RunProjectionsOptions (..) )
import Database.SQLite.Simple   ( open )
import Eventless                ( loadAggregates, loadEvents )
import Eventless.Backend.SQLite ( makeSQLite3Backend )
import Projections              ( handleProjections )

import qualified Options.Applicative as O

--------------------------------------------------------------------------------

runProjections :: RunProjectionsOptions -> IO ()
runProjections _ = do
  -- Create CQRS Backend
  eventSchema      <- open "event.db"
  projectionSchema <- open "pixel.db"
  backend          <- makeSQLite3Backend <$> pure eventSchema

  -- For all Aggregates, load their events and run a projection over their
  -- events into the read store.
  uuids <- loadAggregates backend
  for_ uuids \uuid -> do
    events <- loadEvents backend uuid
    handleProjections projectionSchema uuid events
    pure ()

--------------------------------------------------------------------------------

commandRunProjections :: O.Mod O.CommandFields Options
commandRunProjections = O.command "run-projections"
  (O.info parseOptions
    (O.progDesc "Run Projections from Scratch"))

parseOptions :: O.Parser Options
parseOptions = RunProjections <$> (RunProjectionsOptions <$> fromOption)

fromOption :: O.Parser Int
fromOption = O.option O.auto
  (  O.long  "from"
  <> O.short 'f'
  <> O.help  "From which event to project from"
  <> O.showDefault
  <> O.value 1
  )
