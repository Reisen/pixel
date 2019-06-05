module Pixel.API.AppendImageTags
  ( Route
  , Request(..)
  ) where

import Protolude
import Servant
import Data.Aeson      ( ToJSON(..), FromJSON(..) )
import Pixel.API.Types ( CookieToken )

--------------------------------------------------------------------------------

type Route =
  Header "Cookie" CookieToken
    :> Capture "uuid" Text
    :> "tags"
    :> ReqBody '[JSON] Request
    :> Post '[JSON] NoContent

--------------------------------------------------------------------------------

newtype Request = Request
  { _tags :: [Text]
  } deriving (Show, Generic)

--------------------------------------------------------------------------------

instance ToJSON Request where
instance FromJSON Request where
