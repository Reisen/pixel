module Commands.Types
  ( Options (..)
  , RunOptions (..)
  , GenerateTypesOptions (..)
  , typeScriptOptions
  ) where

--------------------------------------------------------------------------------

import           Protolude

import qualified Data.Aeson                    as A

--------------------------------------------------------------------------------

data Options
  = Run           RunOptions
  | GenerateTypes GenerateTypesOptions

--------------------------------------------------------------------------------

data RunOptions = RunOptions
  { _runOptionsPort    :: Int
  , _runOptionsAddress :: Text
  }

data GenerateTypesOptions = GenerateTypesOptions
  { _generateTypesOptionsFolder :: Text
  }

--------------------------------------------------------------------------------

typeScriptOptions :: A.Options
typeScriptOptions = A.defaultOptions
