module Pixel.API.FetchImageTags
  ( FetchImageTagsRoute
  , FetchImageTagsResponse(..)
  ) where

import Protolude
import Servant
import Data.Aeson      ( ToJSON(..), FromJSON(..) )
import Pixel.API.Types ( CookieToken )

--------------------------------------------------------------------------------

type FetchImageTagsRoute =
  Header "Authorization" CookieToken
    :> Capture "uuid" Text
    :> "tags"
    :> Get '[JSON] FetchImageTagsResponse

--------------------------------------------------------------------------------

newtype FetchImageTagsResponse = FetchImageTagsResponse
  { _tags :: [Text]
  } deriving (Show, Generic)

--------------------------------------------------------------------------------

instance ToJSON FetchImageTagsResponse where
instance FromJSON FetchImageTagsResponse where

