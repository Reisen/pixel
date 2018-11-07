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
  config <- Configuration
    <$> readTextEnv    "IMAGELESS_STATIC"
    <*> readNumericEnv "IMAGELESS_PORT"
    <*> map makeSQLite3Backend (SQLite.open "imageless.db")

  -- Run Application
  run (config ^. port) (server config)
