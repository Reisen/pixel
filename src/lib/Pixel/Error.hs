module Pixel.Error
  ( Error(..)
  , ImageError(..)
  )
where

import Protolude
import Pixel.API.Images.Error ( ImageError(..) )
import Pixel.API.Users.Error  ( UserError(..) )

--------------------------------------------------------------------------------

data AuthenticationError
  = TokenExpired
  | MissingToken
  deriving Show

data Error
  = AuthError  !AuthenticationError
  | ImageError !ImageError
  | UserError  !UserError
  deriving Show
