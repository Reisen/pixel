module Main where

--------------------------------------------------------------------------------

import           Protolude
import           Control.Lens

import qualified API
import qualified Configuration                 as C
import qualified Network.Wai.Handler.Warp      as W
import qualified Database.SQLite.Simple        as S
import qualified Eventless                     as E
import qualified Eventless.Backend.Hook        as E
import qualified Eventless.Backend.SQLite      as E

--------------------------------------------------------------------------------

handleProjections
  :: MonadIO m
  => E.Event
  -> m ()

handleProjections event = do
  putText (show event)
  pure ()

--------------------------------------------------------------------------------

main :: IO ()
main = do
  -- Create configuration from IO.
  config <- C.readConfig $ C.Config
    { C._configStaticLocation = C.readTextEnv "IMAGELESS_STATIC"
    , C._configPort           = C.readNumericEnv "IMAGELESS_PORT"
    , C._configConnection     = S.open "imageless.db"
      <&> E.hookMiddleware handleProjections
      .   E.makeSQLite3Backend
    }

  -- WAI Run Application
  W.run (config ^. C.configPort) (API.server config)
