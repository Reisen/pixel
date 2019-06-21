module Pixel.Services.User
  ( MonadUser(..)
  )
where

import Protolude
import Data.Time               ( UTCTime )
import Data.UUID               ( UUID )
import Pixel.Model.Users.Types ( User, Role, Email, Password )

--------------------------------------------------------------------------------

class MonadUser m where
  createUser      :: UUID -> UUID -> UTCTime -> Email -> Password -> m ()
  findRoleByName  :: Text -> m (Maybe (UUID, Role))
  findUserByUUID  :: UUID -> m (Maybe User)
  findUserByEmail :: Email -> m (Maybe (UUID, User))
  updatePassword  :: UUID -> Password -> m ()