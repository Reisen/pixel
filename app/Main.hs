module Main where

import Protolude
import API                         (server)
import Configuration
import Control.Lens
import Network.Wai.Handler.Warp    (run)
import Database.SQLite.Simple   as SQLite
import Eventless.Backend.SQLite    (makeSQLite3Backend)


main :: IO ()
main = do
  -- Read Configuration
  --
  -- Of interest here is the backend construction, the application uses
  -- event sourcing to manage data. You can swap out backends here and
  -- everything should continue to work.
  --
  -- TODO: Make Backend Choice Configurable
  config <-
    Configuration
      <$> readTextEnv "IMAGELESS_STATIC"
      <*> readNumericEnv "IMAGELESS_PORT"
      <*> map makeSQLite3Backend (SQLite.open "imageless.db")

  -- WAI Run Application
  run (config ^. port) (server config)
