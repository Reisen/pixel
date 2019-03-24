module Main where

--------------------------------------------------------------------------------

import           Protolude

import           Options                        ( Options (..) , parseOptions )
import           Commands.Run                   ( run )
import           Commands.GenerateTypes         ( generateTypes )

--------------------------------------------------------------------------------

main :: IO ()
main = parseOptions >>= \case
  Run options           -> run options
  GenerateTypes options -> generateTypes options
