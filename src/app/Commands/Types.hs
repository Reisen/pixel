module Commands.Types
  ( Options (..)
  , RunOptions (..)
  , GenerateTypesOptions (..)
  ) where

--------------------------------------------------------------------------------

import           Protolude

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
  { _generateTypesOptionsFile :: Text
  }
