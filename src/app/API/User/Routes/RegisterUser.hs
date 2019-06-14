module API.User.Routes.RegisterUser
  ( postRegisterUser
  ) where

import Protolude hiding ( hash )
import Servant

import Crypto.Random          ( getRandomBytes )
import Crypto.Hash            ( Digest, SHA3_224(..), hash )
import Data.UUID              ( toText )
import Data.UUID.V4           ( nextRandom )
import Data.Time              ( getCurrentTime )
import MonadPixel             ( Pixel )
import Pixel                  ( Error(..) )
import Pixel.API.RegisterUser ( Request(..) )
import Pixel.Model.Token      ( Token(..) )
import Pixel.Operations       ( RegisterDetails(..)
                              , AuthenticateDetails(..)
                              , registerUser
                              , authenticateUser
                              )
import Web.Cookie             ( SetCookie(..), defaultSetCookie )

--------------------------------------------------------------------------------

postRegisterUser
  :: Request
  -> Pixel (Headers '[ Header "Set-Cookie" SetCookie ] NoContent)

postRegisterUser Request{..} = do
  -- First Register the User
  uuid                   <- liftIO nextRandom
  createdAt              <- liftIO getCurrentTime
  hashSalt :: ByteString <- liftIO $ getRandomBytes 16
  registerUser RegisterDetails
    { _uuid      = uuid
    , _createdAt = createdAt
    , _email     = _email
    , _password  = _password
    , _salt      = show (hash hashSalt :: Digest SHA3_224)
    }

  -- Validate the user can authenticate.
  mayToken <- authenticateUser $ AuthenticateDetails
    { _email    = _email
    , _password = _password
    }

  case mayToken of
    Nothing            -> throwError UnknownError
    Just (Token token) -> if token /= toText uuid
      then throwError UnknownError
      else pure
        . flip addHeader NoContent
        $ defaultSetCookie
            { setCookieName     = "token"
            , setCookieValue    = toS (toText uuid)
            , setCookiePath     = Just "/"
            , setCookieHttpOnly = True
            }
