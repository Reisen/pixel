module Commands.Run
  ( commandRun
  , run
  ) where

--------------------------------------------------------------------------------

import           Protolude
import           Control.Lens

import           Commands.Types ( Options (..), RunOptions (..) )
import qualified Configuration                 as C
import qualified Database.SQLite.Simple        as S
import qualified Eventless.Backend.Hook        as E
import qualified Eventless.Backend.SQLite      as E
import qualified Network.Wai.Handler.Warp      as W
import qualified Options.Applicative as O
import qualified Projections                   as P
import qualified Server

--------------------------------------------------------------------------------

run :: RunOptions -> IO ()
run _ = do
  -- Create configuration from IO.
  readSchema <- S.open "pixel.db"
  config     <- C.readConfig $ C.Config
    { C._configStaticLocation = C.readTextEnv "PIXEL_STATIC" "tmp/"
    , C._configPort           = C.readNumericEnv "PIXEL_PORT" 6666
    , C._configReadSchema     = pure readSchema
    , C._configConnection     = identity
      .   E.hookMiddleware (P.handleProjections readSchema)
      .   E.makeSQLite3Backend
      <$> S.open "event.db"
    }

  -- WAI Run Application
  W.run (config ^. C.configPort) (Server.server config)

--------------------------------------------------------------------------------

commandRun :: O.Mod O.CommandFields Options
commandRun = O.command "run"
  (O.info parseOptions
    (O.progDesc "Run API"))

parseOptions :: O.Parser Options
parseOptions = Run <$>
  (   RunOptions
  <$> portOption
  <*> addressOption
  )

portOption :: O.Parser Int
portOption = O.option O.auto
  (  O.long  "port"
  <> O.short 'p'
  <> O.help  "Port to listen on."
  <> O.showDefault
  <> O.value 3001
  )

addressOption :: O.Parser Text
addressOption = O.strOption
  (  O.long "address"
  <> O.help "Address to listen on."
  )
