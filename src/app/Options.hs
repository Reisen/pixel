module Options
  ( Options (..)
  , RunOptions
  , GenerateTypesOptions
  , parseOptions
  ) where

--------------------------------------------------------------------------------

import Protolude
import Commands.Run            ( commandRun )
import Commands.RunProjections ( commandRunProjections )
import Commands.GenerateTypes  ( commandGenerateTypes )
import Commands.Types

import qualified Options.Applicative as O

--------------------------------------------------------------------------------

parseOptions :: IO Options
parseOptions = O.execParser $ O.info (commands <**> O.helper)
  (  O.fullDesc
  <> O.progDesc "Pixel!"
  <> O.header "Dots everywhere."
  )

-- Main Option Parser
commands :: O.Parser Options
commands = O.subparser
  (  commandRun
  <> commandRunProjections
  <> commandGenerateTypes
  )
