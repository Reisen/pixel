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

import Options.Applicative     ( Parser
                               , execParser
                               , fullDesc
                               , header
                               , helper
                               , info
                               , progDesc
                               , subparser
                               )


--------------------------------------------------------------------------------

parseOptions :: IO Options
parseOptions = execParser $ info (commands <**> helper)
  (  fullDesc
  <> progDesc "Pixel!"
  <> header "Dots everywhere."
  )

-- Main Option Parser
commands :: Parser Options
commands = subparser
  (  commandRun
  <> commandRunProjections
  <> commandGenerateTypes
  )
