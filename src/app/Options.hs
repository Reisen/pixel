module Options
  ( Options (..)
  , RunOptions
  , GenerateTypesOptions
  , parseOptions
  ) where

--------------------------------------------------------------------------------

import           Protolude
import qualified Options.Applicative as O
import           Commands.Run ( commandRun )
import           Commands.GenerateTypes ( commandGenerateTypes )
import           Commands.Types

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
  <> commandGenerateTypes
  )
