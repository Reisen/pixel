module Pixel.API.Images.Error ( ImageError(..) ) where

--------------------------------------------------------------------------------

import           Protolude

--------------------------------------------------------------------------------

data ImageError
  = MissingToken
  | InvalidUUID
  | ImageDoesNotExist
  deriving Show

