module API.Image.Routes.GetImage
  ( GetImage
  , getImage
  )
where

--------------------------------------------------------------------------------

import           Protolude
import           Servant

import qualified API.Image.Error               as API
import qualified Data.Aeson                    as A
import qualified Error                         as E
import qualified JSON                          as J
import qualified Pixel                         as Pixel
import qualified MonadPixel                    as C

--------------------------------------------------------------------------------

-- We wrap up responses in an HTTP endpoint type, in order to abstract
-- away from the backend.
type GetImage =
  Header "Authorization" Pixel.Token
    :> Get '[JSON] Response

--------------------------------------------------------------------------------

data ImageResponse = ImageResponse
  { imageResponsePath  :: !Text
  , imageResponseThumb :: !Text
  , imageResponseUUID  :: !Pixel.DigestText
  } deriving (Show, Generic)

instance A.ToJSON ImageResponse where
  toEncoding = J.pixelToEncoding
  toJSON     = J.pixelToJSON

newtype Response = Response
  { responseImages :: [ImageResponse]
  } deriving (Show, Generic)

instance A.ToJSON Response where
  toEncoding = J.pixelToEncoding
  toJSON     = J.pixelToJSON

--------------------------------------------------------------------------------

createImageResponse
  :: Pixel.DigestText
  -> Pixel.Image
  -> ImageResponse

createImageResponse uuid Pixel.Image{..} = ImageResponse
  { imageResponsePath  = fold ["/static/images/", _imageHash]
  , imageResponseThumb = fold ["/static/thumbs/", _imageHash]
  , imageResponseUUID  = uuid
  }

--------------------------------------------------------------------------------

-- Fetch images from the backend, this endpoint accepts a filter to decide what
-- to return from the backend. By default this is all images ordered by upload
-- date and the filter should encompass all possible queries a user might have,
-- from tags to uploader, to ordering.
getImage
  :: Maybe Pixel.Token
  -> C.Pixel Response

getImage Nothing  = throwError (E.ImageError API.MissingToken)
getImage (Just _) = handleImagesRquest

--------------------------------------------------------------------------------

handleImagesRquest
  :: Monad m
  => Pixel.MonadImage m
  => m Response

handleImagesRquest = do
  images <- Pixel.loadImages 10
  pure Response
    { responseImages = map (uncurry createImageResponse . first show) images
    }
