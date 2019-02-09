module Main where

--------------------------------------------------------------------------------

import           Protolude
import           Control.Lens

import qualified API
import qualified Configuration                 as C
import qualified Database.SQLite.Simple        as S
import qualified Eventless.Backend.Hook        as E
import qualified Eventless.Backend.SQLite      as E
import qualified Network.Wai.Handler.Warp      as W
import qualified Projections                   as P

--------------------------------------------------------------------------------

main :: IO ()
main = do
  -- Create configuration from IO.
  readSchema <- S.open "pixel.db"
  config     <- C.readConfig $ C.Config
    { C._configStaticLocation = C.readTextEnv "PIXEL_STATIC" "tmp/"
    , C._configPort           = C.readNumericEnv "PIXEL_PORT" 6666
    , C._configReadSchema     = pure readSchema
    , C._configConnection     = S.open "event.db"
      >>= pure
      .   E.hookMiddleware (P.handleProjections readSchema)
      .   E.makeSQLite3Backend
    }

  -- WAI Run Application
  W.run (config ^. C.configPort) (API.server config)
