module Pixel.API.Users.Operations.Authentication
  ( AuthenticateDetails(..)
  , RegisterDetails(..)
  , handleAuthenticateUser
  , handleRegisterUser
  ) where

import Protolude
import Control.Lens
import Control.Monad.Trans.Maybe      ( MaybeT(..), runMaybeT )
import Data.Time                      ( UTCTime )
import Data.UUID                      ( UUID, toText )
import Pixel.API.Token                ( Token(..) )
import Pixel.API.Users.Types.Email    ( Email(..) )
import Pixel.API.Users.Types.User     ( UserState(..), userPassword, userState )
import Pixel.API.Users.Types.Password ( VerifyPasswordResult(..)
                                      , makePassword
                                      , verifyPassword
                                      )
import Pixel.Services.User            ( MonadUser(..) )

--------------------------------------------------------------------------------

data AuthenticateDetails = AuthenticateDetails
  { _adEmail    :: !Text
  , _adPassword :: !Text
  } deriving (Show)

handleAuthenticateUser
  :: Monad m
  => MonadIO m
  => MonadUser m
  => AuthenticateDetails
  -> m (Maybe Token)

handleAuthenticateUser AuthenticateDetails{..} =
  runMaybeT $ do
    (uuid, user) <- MaybeT $ traceShowId <$> findUserByEmail (Email _adEmail)
    password     <- MaybeT $ traceShowId <$> pure (user ^. userPassword)
    let passwordCheck = verifyPassword password _adPassword
    let statusCheck   = user ^. userState
    putText "End"
    MaybeT . pure $ case (passwordCheck, statusCheck) of
      (_,               Banned) -> traceShowId Nothing
      (VerifySucceeded, _     ) -> traceShowId . Just . Token . toText $ uuid
      (_,               _     ) -> traceShowId Nothing

--------------------------------------------------------------------------------

data RegisterDetails = RegisterDetails
  { _rdUUID      :: !UUID
  , _rdCreatedAt :: !UTCTime
  , _rdEmail     :: !Text
  , _rdPassword  :: !Text
  , _rdSalt      :: !Text
  } deriving (Show)

handleRegisterUser
  :: Monad m
  => MonadUser m
  => RegisterDetails
  -> m ()

handleRegisterUser RegisterDetails{..} = do
  let email       = Email _rdEmail
  let mayPassword = makePassword _rdPassword _rdSalt
  mayRole <- findRoleByName "Standard"
  case (mayPassword, mayRole) of
    (Just password, Just role) -> createUser _rdUUID (fst role) _rdCreatedAt email password
    (_, _)                     -> pure ()
