module Pixel.API.FetchImages
  ( Route
  , Response(..)
  , GalleryImage(..)
  ) where

import Protolude
import Servant
import Data.Aeson      ( ToJSON(..), FromJSON(..) )
import Pixel.API.Types ( CookieToken )

--------------------------------------------------------------------------------

type Route =
  Header "Cookie" CookieToken
    :> Get '[JSON] Response

--------------------------------------------------------------------------------

data GalleryImage = GalleryImage
  { _dimensions :: (Int, Int)
  , _filename   :: !Text
  , _filesize   :: !Int
  , _path       :: !Text
  , _tags       :: ![Text]
  , _thumb      :: !Text
  , _uuid       :: !Text
  } deriving (Show, Generic)


newtype Response = Response
  { _images :: [GalleryImage]
  } deriving (Show, Generic)

--------------------------------------------------------------------------------

instance ToJSON GalleryImage where
instance ToJSON Response where
instance FromJSON GalleryImage where
instance FromJSON Response where
