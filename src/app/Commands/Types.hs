module Commands.Types
  ( Commands.Types.Options (..)
  , RunOptions (..)
  , RunProjectionsOptions (..)
  , GenerateTypesOptions (..)
  , named
  ) where

--------------------------------------------------------------------------------

import Protolude
import Data.Aeson as Aeson ( Options(..), defaultOptions )

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

--------------------------------------------------------------------------------

-- Because all our Request/Response types are named Request/Response in our
-- route files, the generated TS results in a bunch of conflicting type names.
-- To get around this, we'll hand-name the output types with a helper function
-- to generate options:

named :: Text -> Aeson.Options
named name = defaultOptions
  { constructorTagModifier = toS . const name
  , fieldLabelModifier     = dropWhile (=='_')
  }
