module Pixel.Error
  ( Error(..)
  , ImageError(..)
  )
where

--------------------------------------------------------------------------------

import           Protolude

import           Pixel.API.Images.Error         ( ImageError(..) )

--------------------------------------------------------------------------------

data AuthenticationError
  = TokenExpired
  | MissingToken
  deriving Show

data Error
  = ImageError ImageError
  | AuthError  AuthenticationError
  deriving Show
