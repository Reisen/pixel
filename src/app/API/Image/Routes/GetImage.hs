module API.Image.Routes.GetImage
  ( GetImage
  , GetImageResponse
  , getImage
  )
where

--------------------------------------------------------------------------------

import Protolude
import Servant

import API.Image.Types as API         ( Image(..) )
import Data.Aeson                     ( ToJSON (..) )
import Pixel as Pixel                 ( DigestText
                                      , Error (..)
                                      , Image (..)
                                      , ImageError (..)
                                      , Token
                                      , fetchImages
                                      , pixelToEncoding
                                      , pixelToJSON
                                      )
import MonadPixel                     ( Pixel )

--------------------------------------------------------------------------------

-- We wrap up responses in an HTTP endpoint type, in order to abstract
-- away from the backend.
type GetImage =
  Header "Authorization" Token
    :> Get '[JSON] GetImageResponse

--------------------------------------------------------------------------------

newtype GetImageResponse = GetImageResponse
  { getImageResponseImages :: [API.Image]
  } deriving (Show, Generic)

instance ToJSON GetImageResponse where
  toEncoding = pixelToEncoding
  toJSON     = pixelToJSON

--------------------------------------------------------------------------------

convertImage
  :: DigestText
  -> Pixel.Image
  -> API.Image

convertImage uuid Pixel.Image{..} = API.Image
  { imagePath  = fold ["/static/images/", _imageHash]
  , imageThumb = fold ["/static/thumbs/", _imageHash]
  , imageUUID  = uuid
  , imageTags  = _imageTags
  }

--------------------------------------------------------------------------------

-- Fetch images from the backend, this endpoint accepts a filter to decide what
-- to return from the backend. By default this is all images ordered by upload
-- date and the filter should encompass all possible queries a user might have,
-- from tags to uploader, to ordering.
getImage
  :: Maybe Token
  -> Pixel GetImageResponse

getImage Nothing  = throwError (ImageError MissingToken)
getImage (Just _) = do
  images <- fetchImages
  pure GetImageResponse
    { getImageResponseImages = map (uncurry convertImage . first show) images
    }
