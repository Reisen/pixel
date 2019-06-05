module Pixel.API.FetchImageTags
  ( Route
  , Response(..)
  ) where

import Protolude
import Servant
import Data.Aeson      ( ToJSON(..), FromJSON(..) )
import Pixel.API.Types ( CookieToken )

--------------------------------------------------------------------------------

type Route =
  Header "Authorization" CookieToken
    :> Capture "uuid" Text
    :> "tags"
    :> Get '[JSON] Response

--------------------------------------------------------------------------------

newtype Response = Response
  { _tags :: [Text]
  } deriving (Show, Generic)

--------------------------------------------------------------------------------

instance ToJSON Response where
instance FromJSON Response where

