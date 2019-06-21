module Pixel.API.UpdatePasswordUser
  ( Route
  , Request(..)
  ) where

import Protolude
import Servant
import Data.Aeson        ( ToJSON(..), FromJSON(..) )
import Pixel.API.Types   ( CookieToken )

--------------------------------------------------------------------------------

type Route =
  Header "Cookie" CookieToken
    :> "password"
    :> ReqBody '[JSON] Request
    :> Post '[JSON] NoContent

--------------------------------------------------------------------------------

data Request = Request
  { _currentPassword :: Text
  , _newPassword     :: Text
  } deriving (Generic)

--------------------------------------------------------------------------------

instance ToJSON   Request where
instance FromJSON Request where