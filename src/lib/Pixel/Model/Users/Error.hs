module Pixel.Model.Users.Error
  ( AuthenticationFailedReason(..)
  , RegistrationFailedReason(..)
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

data RegistrationFailedReason
  = AccountExists
  | OtherRegistrationFailure !Text
  deriving Show

data UserError
  = MissingToken
  | InvalidUUID
  | AuthenticationFailed !AuthenticationFailedReason
  | RegistrationFailed !RegistrationFailedReason
  deriving Show
