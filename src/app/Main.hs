module Main where

--------------------------------------------------------------------------------

import Protolude

import Options                 ( Options (..) , parseOptions )
import Commands.Run            ( run )
import Commands.RunProjections ( runProjections )
import Commands.GenerateTypes  ( generateTypes )

--------------------------------------------------------------------------------

main :: IO ()
main = parseOptions >>= \case
  Run opts            -> run opts
  RunProjections opts -> runProjections opts
  GenerateTypes opts  -> generateTypes opts
