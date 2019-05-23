module API.User.Routes.RegisterUser
  ( RegisterUser
  , RegisterRequest
  , postRegisterUser
  ) where

import Protolude hiding ( hash )
import Servant

import Crypto.Random       ( getRandomBytes )
import Crypto.Hash         ( Digest, SHA3_224(..), hash )
import Data.Aeson          ( FromJSON(..) )
import Data.UUID           ( toText )
import Data.UUID.V4        ( nextRandom )
import Data.Time           ( getCurrentTime )
import MonadPixel          ( Pixel )
import Pixel               ( pixelParseJSON )
import Pixel.API.Users     ( RegisterDetails(..)
                           , handleRegisterUser
                           )
import Web.Cookie          ( SetCookie(..), defaultSetCookie )

--------------------------------------------------------------------------------

-- Wrap up Token with a Set-Cookie header, this is so rather than storing the
-- token in a JS accessible place we can secure the cookie with `Secure` and
-- `HttpsOnly` to prevent XSS.
type TokenInHeader = Headers
  '[ Header "Set-Cookie" SetCookie
   ] NoContent

type RegisterUser =
  ReqBody '[JSON] RegisterRequest
    :> Post '[JSON] TokenInHeader

--------------------------------------------------------------------------------

data RegisterRequest = RegisterRequest
  { _registerRequestEmail    :: Text
  , _registerRequestPassword :: Text
  } deriving (Show, Generic)

instance FromJSON RegisterRequest where
  parseJSON = pixelParseJSON

--------------------------------------------------------------------------------

postRegisterUser
  :: RegisterRequest
  -> Pixel TokenInHeader

postRegisterUser RegisterRequest{..} = do
  uuid                   <- liftIO nextRandom
  createdAt              <- liftIO getCurrentTime
  hashSalt :: ByteString <- liftIO $ getRandomBytes 16
  handleRegisterUser RegisterDetails
    { _rdUUID      = uuid
    , _rdCreatedAt = createdAt
    , _rdEmail     = _registerRequestEmail
    , _rdPassword  = _registerRequestPassword
    , _rdSalt      = show (hash hashSalt :: Digest SHA3_224)
    }

  pure
    . flip addHeader NoContent
    $ defaultSetCookie
        { setCookieName     = "token"
        , setCookieValue    = toS (toText uuid)
        , setCookiePath     = Just "/"
        -- , setCookieHttpOnly = True
        }
