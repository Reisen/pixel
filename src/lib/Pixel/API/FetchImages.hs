module Pixel.API.FetchImages
  ( FetchImagesRoute
  , FetchImagesResponse(..)
  , GalleryImage(..)
  ) where

import Protolude
import Servant
import Data.Aeson      ( ToJSON(..), FromJSON(..) )
import Pixel.API.Types ( CookieToken )

--------------------------------------------------------------------------------

type FetchImagesRoute =
  Header "Cookie" CookieToken
    :> Get '[JSON] FetchImagesResponse

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


newtype FetchImagesResponse = FetchImagesResponse
  { _images :: [GalleryImage]
  } deriving (Show, Generic)

--------------------------------------------------------------------------------

instance ToJSON GalleryImage where
instance ToJSON FetchImagesResponse where
instance FromJSON GalleryImage where
instance FromJSON FetchImagesResponse where
