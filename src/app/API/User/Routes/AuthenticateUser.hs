module API.User.Routes.AuthenticateUser
  ( postAuthenticateUser
  ) where

import Protolude
import Servant
import Data.Binary.Builder        ( toLazyByteString )
import MonadPixel                 ( Pixel )
import Pixel                      ( Error(..) )
import Pixel.API.AuthenticateUser ( Request(..) )
import Pixel.Model.Token          ( Token(..) )
import Pixel.Operations           ( AuthenticateDetails(..), authenticateUser )
import Web.Cookie                 ( SetCookie(..), renderSetCookie, defaultSetCookie )

--------------------------------------------------------------------------------

postAuthenticateUser
  :: Request
  -> Pixel (Headers '[ Header "Set-Cookie" Text ] NoContent)

postAuthenticateUser Request{..} = do
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
      { _email    = _email
      , _password = _password
      }
