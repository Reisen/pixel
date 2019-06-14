module Pixel.API.AuthenticateUser
  ( Route
  , Request(..)
  ) where

import Protolude
import Servant
import Data.Aeson ( FromJSON(..) )

--------------------------------------------------------------------------------

-- Wrap up Token with a Set-Cookie header, this is so rather than storing the
-- token in a JS accessible place we can secure the cookie with `Secure` and
-- `HttpsOnly` to prevent XSS.

type Route =
  "login"
    :> ReqBody '[JSON] Request
    :> Post '[JSON] (Headers
    '[ Header "Set-Cookie" Text
     ] NoContent)

--------------------------------------------------------------------------------

data Request = Request
  { _email    :: !Text
  , _password :: !Text
  } deriving (Show, Generic)

--------------------------------------------------------------------------------

instance FromJSON Request where
