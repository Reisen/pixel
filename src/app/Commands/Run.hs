module Commands.Run
  ( commandRun
  , Commands.Run.run
  ) where

import Protolude
import Configuration
import Control.Lens
import Server

import Commands.Types                   ( Options (..), RunOptions (..) )
import Database.SQLite.Simple           ( open )
import Eventless.Backend.Hook           ( hookMiddleware )
import Eventless.Backend.SQLite         ( makeSQLite3Backend )
import Network.Wai.Handler.Warp as Warp ( run )
import Options.Applicative as A         ( Mod
                                        , CommandFields
                                        , Parser
                                        , auto
                                        , command
                                        , help
                                        , info
                                        , long
                                        , option
                                        , progDesc
                                        , short
                                        , showDefault
                                        , strOption
                                        , value
                                        )
import Projections                      ( prepareProjections, handleProjections )

--------------------------------------------------------------------------------

run :: RunOptions -> IO ()
run _ = do
  -- Create configuration from IO.
  readSchema  <- open "pixel.db"
  writeSchema <- open "event.db"
  config      <- readConfig $ Config
    { _configStaticLocation = readTextEnv "PIXEL_STATIC" "tmp/"
    , _configPort           = readNumericEnv "PIXEL_PORT" 6666
    , _configReadSchema     = pure readSchema
    , _configConnection     = pure
        . hookMiddleware (handleProjections readSchema)
        $ makeSQLite3Backend writeSchema
    }

  -- Prepare Projection Tables
  prepareProjections readSchema

  -- WAI Run Application
  Warp.run (config ^. configPort) (server config)

--------------------------------------------------------------------------------

commandRun :: Mod CommandFields Options
commandRun = command "run"
  (info parseOptions
    (progDesc "Run API"))

parseOptions :: Parser Options
parseOptions = Run <$>
  (   RunOptions
  <$> portOption
  <*> addressOption
  )

--------------------------------------------------------------------------------

portOption :: Parser Int
portOption = A.option auto
  (  long  "port"
  <> short 'p'
  <> help  "Port to listen on."
  <> showDefault
  <> value 3001
  )

addressOption :: Parser Text
addressOption = strOption
  (  long "address"
  <> help "Address to listen on."
  )
