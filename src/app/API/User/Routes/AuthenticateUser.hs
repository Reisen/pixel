module API.User.Routes.AuthenticateUser
  ( AuthenticateUser
  , AuthUserRequest(..)
  , postAuthenticateUser
  ) where

import Protolude
import Servant

import Data.Aeson      ( FromJSON(..) )
import Pixel           ( Error(..), pixelParseJSON )
import Pixel.API.Token ( Token )
import Pixel.API.Users ( UserError(..)
                       , AuthenticationFailedReason(..)
                       , AuthenticateDetails(..)
                       , handleAuthenticateUser
                       )
import MonadPixel      ( Pixel )

--------------------------------------------------------------------------------

type AuthenticateUser =
  "login"
    :> ReqBody '[JSON] AuthUserRequest
    :> Post '[JSON] Token

--------------------------------------------------------------------------------

data AuthUserRequest = AuthUserRequest
  { _authUserRequestEmail    :: Text
  , _authUserRequestPassword :: Text
  } deriving (Show, Generic)

instance FromJSON AuthUserRequest where
  parseJSON = pixelParseJSON

--------------------------------------------------------------------------------

postAuthenticateUser
  :: AuthUserRequest
  -> Pixel Token

postAuthenticateUser AuthUserRequest{..} = do
  mayToken <- handleAuthenticateUser $ AuthenticateDetails
    { _adEmail    = _authUserRequestEmail
    , _adPassword = _authUserRequestPassword
    }

  case mayToken of
    Nothing    -> throwError (UserError . AuthenticationFailed $ OtherAuthFailure "Unknown")
    Just token -> pure token
