module Pixel.Operations.UpdatePassword
  (
    UpdatePasswordDetails(..)
  , updateUserPassword
  ) where

import Protolude
import Data.UUID                        ( UUID )
import Pixel.Services.User              ( findUserByUUID
                                        , updatePassword
                                        , MonadUser
                                        )
import Pixel.Model.Users.Types.User     ( User(..) )
import Pixel.Model.Users.Types.Password ( verifyPassword 
                                        , makePassword
                                        , VerifyPasswordResult(..)
                                        )
--------------------------------------------------------------------------------

data UpdatePasswordDetails = UpdatePasswordDetails 
  { _uuid            :: UUID
  , _currentPassword :: Text
  , _newPassword     :: Text
  , _salt            :: Text
  }

--------------------------------------------------------------------------------
updateUserPassword
  :: Monad m
  => Applicative m
  => MonadUser m           
  => UpdatePasswordDetails 
  -> m ()                  

updateUserPassword UpdatePasswordDetails{..} = do
  mayUser <- findUserByUUID _uuid
  case mayUser of
    Nothing -> pure ()
    Just user -> case (_password user) of
      Just pwd -> 
        case verifyPassword pwd _currentPassword of
         VerifySucceeded -> case makePassword _newPassword _salt of
           Nothing -> pure ()
           Just password -> do
             updatePassword _uuid password
             pure ()
         _ -> pure ()
      Nothing -> pure () 
--------------------------------------------------------------------------------