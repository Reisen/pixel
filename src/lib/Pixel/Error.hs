module Pixel.Error
  ( Error(..)
  , ImageError(..)
  )
where

import Protolude
import Pixel.Model.Images.Error ( ImageError(..) )
import Pixel.Model.Users.Error  ( UserError(..) )

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
