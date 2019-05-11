module Pixel.API.Users.Error
  ( AuthenticationFailedReason(..)
  , UserError(..)
  ) where

import Protolude

--------------------------------------------------------------------------------

data AuthenticationFailedReason
  = UserDoesNotExist
  | PasswordIncorrect
  | AccountInaccessible
  | OtherAuthFailure !Text
  deriving Show

data UserError
  = MissingToken
  | InvalidUUID
  | AuthenticationFailed !AuthenticationFailedReason
  deriving Show
