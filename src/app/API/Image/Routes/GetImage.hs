module API.Image.Routes.GetImage
  ( GetImage
  , GetImageResponse
  , getImage
  )
where

--------------------------------------------------------------------------------

import Protolude
import Servant

import API.Cookies               ( CookieList(..) )
import API.Image.Types as API    ( Image(..) )
import Data.Aeson                ( ToJSON(..) )
import MonadPixel                ( Pixel )
import Pixel                     ( Error(..), pixelToEncoding, pixelToJSON )
-- import Pixel.API.Token           ( Token )
import Pixel.API.Images as Pixel ( Image(..), ImageError(..), DigestText, fetchImages )

--------------------------------------------------------------------------------

-- We wrap up responses in an HTTP endpoint type, in order to abstract
-- away from the backend.
type GetImage =
  Header "Cookie" CookieList
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
  :: Maybe CookieList
  -> Pixel GetImageResponse

getImage Nothing                     = throwError (ImageError MissingToken)
getImage (Just (CookieList cookies)) = do
  for_ cookies $ \(name, value) -> do
    putText $ fold ["Cookie: ", name, " -- ", value]
    pure ()

  images <- fetchImages
  pure GetImageResponse
    { getImageResponseImages =
        map (uncurry convertImage . first show) images
    }
