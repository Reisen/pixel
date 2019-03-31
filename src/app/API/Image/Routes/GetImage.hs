module API.Image.Routes.GetImage
  ( GetImage
  , GetImageResponse
  , getImage
  )
where

--------------------------------------------------------------------------------

import           Protolude
import           Servant

import           API.Image.Types                ( Image(..) )
import qualified Data.Aeson                    as A
import qualified Pixel                         as Pixel
import qualified MonadPixel                    as C

--------------------------------------------------------------------------------

-- We wrap up responses in an HTTP endpoint type, in order to abstract
-- away from the backend.
type GetImage =
  Header "Authorization" Pixel.Token
    :> Get '[JSON] GetImageResponse

--------------------------------------------------------------------------------

newtype GetImageResponse = GetImageResponse
  { getImageResponseImages :: [Image]
  } deriving (Show, Generic)

instance A.ToJSON GetImageResponse where
  toEncoding = Pixel.pixelToEncoding
  toJSON     = Pixel.pixelToJSON

--------------------------------------------------------------------------------

convertImage
  :: Pixel.DigestText
  -> Pixel.Image
  -> Image

convertImage uuid Pixel.Image{..} = Image
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
  :: Maybe Pixel.Token
  -> C.Pixel GetImageResponse

getImage Nothing  = throwError (Pixel.ImageError Pixel.MissingToken)
getImage (Just _) = do
  images <- Pixel.fetchImages
  pure GetImageResponse
    { getImageResponseImages = map (uncurry convertImage . first show) images
    }
