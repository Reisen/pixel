module API.User.Routes.AuthenticateUser
  ( AuthenticateUser
  , AuthUserRequest(..)
  , postAuthenticateUser
  ) where

import Protolude
import Servant
import Data.Aeson          ( FromJSON(..) )
import Data.Binary.Builder ( toLazyByteString )
import MonadPixel          ( Pixel )
import Pixel               ( Error(..), pixelParseJSON )
import Pixel.Model.Token   ( Token(..) )
import Pixel.Operations    ( AuthenticateDetails(..), authenticateUser )
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
  mayToken <- authenticateUser authRequest
  case mayToken of
    Nothing    -> throwError UnknownError
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

  where
    authRequest = AuthenticateDetails
      { _email    = _authUserRequestEmail
      , _password = _authUserRequestPassword
      }
