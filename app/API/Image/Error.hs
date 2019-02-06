module API.Image.Error
  ( ImageError(..)
  )
where

--------------------------------------------------------------------------------

import           Protolude

--------------------------------------------------------------------------------

data ImageError
  = MissingToken
  | InvalidUUID
  | ImageDoesNotExist
  deriving Show
