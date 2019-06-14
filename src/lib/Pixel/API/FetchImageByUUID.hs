module Pixel.API.FetchImageByUUID
  ( Route
  , Response(..)
  ) where

import Protolude
import Servant
import Data.Aeson      ( ToJSON(..), FromJSON(..) )
import Pixel.API.Types ( CookieToken, APIImage )

--------------------------------------------------------------------------------

type Route =
  Header "Authorization" CookieToken
    :> Capture "uuid" Text
    :> Get '[JSON] Response

--------------------------------------------------------------------------------

data Response = Response
  { _image :: APIImage
  } deriving (Show, Generic)

--------------------------------------------------------------------------------

instance ToJSON Response where
instance FromJSON Response where
