module Pixel.API.FetchImageByUUID
  ( FetchImageByUUIDRoute
  , FetchImageByUUIDResponse(..)
  ) where

import Protolude
import Servant
import Data.Aeson      ( ToJSON(..), FromJSON(..) )
import Pixel.API.Types ( CookieToken )

--------------------------------------------------------------------------------

type FetchImageByUUIDRoute =
  Header "Authorization" CookieToken
    :> Capture "uuid" Text
    :> Get '[JSON] FetchImageByUUIDResponse

--------------------------------------------------------------------------------

data FetchImageByUUIDResponse = FetchImageByUUIDResponse
  { _dimensions :: (Int, Int)
  , _filename   :: !Text
  , _filesize   :: !Int
  , _path       :: !Text
  , _tags       :: ![Text]
  , _thumb      :: !Text
  , _uuid       :: !Text
  } deriving (Show, Generic)

--------------------------------------------------------------------------------

instance ToJSON FetchImageByUUIDResponse where
instance FromJSON FetchImageByUUIDResponse where
