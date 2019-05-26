module API.User.Routes.AuthenticateUser
  ( AuthenticateUser
  , AuthUserRequest(..)
  , postAuthenticateUser
  ) where

import Protolude
import Servant

import Data.Aeson          ( FromJSON(..) )
import Data.Binary.Builder ( toLazyByteString )
import Pixel               ( Error(..), pixelParseJSON )
import Pixel.API.Token     ( Token(..) )
import Pixel.API.Users     ( UserError(..)
                           , AuthenticationFailedReason(..)
                           , AuthenticateDetails(..)
                           , handleAuthenticateUser
                           )
import MonadPixel          ( Pixel )
import Web.Cookie          ( SetCookie(..), renderSetCookie, defaultSetCookie )

--------------------------------------------------------------------------------

-- Wrap up Token with a Set-Cookie header, this is so rather than storing the
-- token in a JS accessible place we can secure the cookie with `Secure` and
-- `HttpsOnly` to prevent XSS.
type TokenInHeader = Headers
  '[ Header "Set-Cookie" Text
   ] NoContent

type AuthenticateUser =
  "login"
    :> ReqBody '[JSON] AuthUserRequest
    :> Post '[JSON] TokenInHeader

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
  -> Pixel TokenInHeader

postAuthenticateUser AuthUserRequest{..} = do
  mayToken <- handleAuthenticateUser $ AuthenticateDetails
    { _adEmail    = _authUserRequestEmail
    , _adPassword = _authUserRequestPassword
    }

  case mayToken of
    Nothing    -> throwError (UserError . AuthenticationFailed $ OtherAuthFailure "Unknown")
    Just token -> pure
      . flip addHeader NoContent
      . toS
      . toLazyByteString
      . renderSetCookie
      $ defaultSetCookie
          { setCookieName     = "token"
          , setCookieValue    = toS (_tokenText token)
          , setCookiePath     = Just "/"
          , setCookieHttpOnly = True
          }
