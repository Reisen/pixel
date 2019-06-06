module Pixel.Error
  ( Error(..)
  ) where

import Protolude
import Pixel.Model.Users.Error  ( UserError(..) )

--------------------------------------------------------------------------------

data AuthenticationError
  = TokenExpired
  | MissingToken
  deriving Show

data Error
  = AuthError  !AuthenticationError
  | UserError  !UserError
  | UnknownError
  deriving Show
