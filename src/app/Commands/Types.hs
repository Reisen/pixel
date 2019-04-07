module Commands.Types
  ( Options (..)
  , RunOptions (..)
  , RunProjectionsOptions (..)
  , GenerateTypesOptions (..)
  ) where

--------------------------------------------------------------------------------

import           Protolude

--------------------------------------------------------------------------------

data Options
  = Run            RunOptions
  | RunProjections RunProjectionsOptions
  | GenerateTypes  GenerateTypesOptions

--------------------------------------------------------------------------------

data RunOptions = RunOptions
  { _runOptionsPort    :: Int
  , _runOptionsAddress :: Text
  }

data RunProjectionsOptions = RunProjectionsOptions
  { _runProjectionsOptionsFrom :: Int
  }

data GenerateTypesOptions = GenerateTypesOptions
  { _generateTypesOptionsFile :: Text
  }
