module Pixel.Model.Users.Operations.Authentication
  ( AuthenticateDetails(..)
  , RegisterDetails(..)
  , handleAuthenticateUser
  , handleRegisterUser
  ) where

import Protolude hiding                 ( state)
import Pixel.Lens
import Control.Lens
import Control.Monad.Trans.Maybe        ( MaybeT(..), runMaybeT )
import Data.Time                        ( UTCTime )
import Data.UUID                        ( UUID, toText )
import Pixel.Model.Token                ( Token(..) )
import Pixel.Model.Users.Types.Email    ( Email(..) )
import Pixel.Model.Users.Types.User     ( UserState(..) )
import Pixel.Model.Users.Types.Password ( VerifyPasswordResult(..)
                                        , makePassword
                                        , verifyPassword
                                        )
import Pixel.Services.User              ( MonadUser(..) )

--------------------------------------------------------------------------------

data AuthenticateDetails = AuthenticateDetails
  { _email    :: !Text
  , _password :: !Text
  } deriving (Show)

handleAuthenticateUser
  :: Monad m
  => MonadIO m
  => MonadUser m
  => AuthenticateDetails
  -> m (Maybe Token)

handleAuthenticateUser AuthenticateDetails{..} = runMaybeT $ do
  (userUUID, user) <- MaybeT $ findUserByEmail (Email _email)
  currentPassword  <- MaybeT $ pure (user ^. password)
  let passwordCheck = verifyPassword currentPassword _password
  let statusCheck   = user ^. state
  putText "End"
  MaybeT . pure $ case (passwordCheck, statusCheck) of
    (_,               Banned) -> Nothing
    (VerifySucceeded, _     ) -> Just . Token . toText $ userUUID
    (_,               _     ) -> Nothing

--------------------------------------------------------------------------------

data RegisterDetails = RegisterDetails
  { _uuid      :: !UUID
  , _createdAt :: !UTCTime
  , _email     :: !Text
  , _password  :: !Text
  , _salt      :: !Text
  } deriving (Show)

handleRegisterUser
  :: Monad m
  => MonadUser m
  => RegisterDetails
  -> m ()

handleRegisterUser RegisterDetails{..} = do
  let newEmail    = Email _email
  let mayPassword = makePassword _password _salt
  mayRole <- findRoleByName "Standard"
  case (mayPassword, mayRole) of
    (Just newPassword, Just defaultRole) -> createUser _uuid (fst defaultRole) _createdAt newEmail newPassword
    (_, _)                               -> pure ()
