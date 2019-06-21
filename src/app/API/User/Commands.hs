module API.User.Commands
  ( createUser
  , changePassword
  ) where

import Protolude
import Data.Time         ( UTCTime )
import Data.UUID         ( UUID )
import Eventless         ( Command, emit )
import Pixel.Model.Users ( User(..), UserEvent(..), Email, Password )

--------------------------------------------------------------------------------

createUser
  :: Monad m
  => UUID
  -> Email
  -> Password
  -> UTCTime
  -> Command User m

createUser roleUUID email password createdAt = do
  emit (EmailChanged email)
  emit (PasswordChanged password)
  emit (RoleChanged roleUUID)
  emit (CreatedAt createdAt)

changePassword
  :: Monad m
  => Password
  -> Command User m

changePassword password = do
  emit (PasswordChanged password)