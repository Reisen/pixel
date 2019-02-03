module Main where

--------------------------------------------------------------------------------

import           Protolude
import           Control.Lens

import qualified API                           as API
import qualified Configuration                 as C
import qualified Network.Wai.Handler.Warp      as W
import qualified Database.SQLite.Simple        as S
import qualified Eventless.Backend.SQLite      as E

--------------------------------------------------------------------------------

handleProjections
  :: MonadIO m
  => Text
  -> m ()

handleProjections event = do
  putText event
  pure ()

--------------------------------------------------------------------------------

main :: IO ()
main = do
  -- Create configuration from IO.
  config <- C.readConfig $ C.Config
    { C._configStaticLocation = C.readTextEnv "IMAGELESS_STATIC"
    , C._configPort           = C.readNumericEnv "IMAGELESS_PORT"
    , C._configConnection     = E.makeSQLite3Backend handleProjections <$> S.open "imageless.db"
    }

  -- WAI Run Application
  W.run (config ^. C.configPort) (API.server config)
