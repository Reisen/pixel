module Pixel.API.FetchImages
  ( Route
  , Response(..)
  , APIImage(..)
  ) where

import Protolude
import Servant
import Data.Aeson      ( ToJSON(..), FromJSON(..) )
import Pixel.API.Types ( CookieToken, APIImage(..) )

--------------------------------------------------------------------------------

type Route =
  Header "Cookie" CookieToken
    :> Get '[JSON] Response

--------------------------------------------------------------------------------

newtype Response = Response
  { _images :: [APIImage]
  } deriving (Show, Generic)

--------------------------------------------------------------------------------

instance ToJSON Response where
instance FromJSON Response where
