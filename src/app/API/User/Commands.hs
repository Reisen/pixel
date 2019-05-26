module API.User.Commands
  ( createUser
  ) where

import Protolude

import Data.Time       ( UTCTime )
import Data.UUID       ( UUID )
import Eventless       ( Command, emit )
import Pixel.API.Users ( User(..)
                       , UserEvent(..)
                       , Email
                       , Password
                       )

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
