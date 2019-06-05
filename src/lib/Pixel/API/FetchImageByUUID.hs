module Pixel.API.FetchImageByUUID
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
    :> Get '[JSON] Response

--------------------------------------------------------------------------------

data Response = Response
  { _dimensions :: (Int, Int)
  , _filename   :: !Text
  , _filesize   :: !Int
  , _path       :: !Text
  , _tags       :: ![Text]
  , _thumb      :: !Text
  , _uuid       :: !Text
  } deriving (Show, Generic)

--------------------------------------------------------------------------------

instance ToJSON Response where
instance FromJSON Response where
