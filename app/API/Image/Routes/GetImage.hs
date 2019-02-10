module API.Image.Routes.GetImage
  ( GetImage
  , getImage
  )
where

--------------------------------------------------------------------------------

import           Protolude
import           Servant

import qualified API.Image.Error               as API
import qualified API.Image.Services            as API
import qualified API.Image.Types               as API
import qualified API.Token                     as API
import qualified Configuration                 as C
import qualified Data.Aeson                    as A
import qualified Error                         as E
import qualified JSON                          as J

--------------------------------------------------------------------------------

-- We wrap up responses in an HTTP endpoint type, in order to abstract
-- away from the backend.
type GetImage =
  Header "Authorization" API.Token
    :> Get '[JSON] Response

--------------------------------------------------------------------------------

data ImageResponse = ImageResponse
  { imageResponsePath  :: !Text
  , imageResponseThumb :: !Text
  , imageResponseUUID  :: !API.DigestText
  } deriving (Show, Generic)

instance A.ToJSON ImageResponse where
  toEncoding = A.genericToEncoding J.pixelJsonEncoding

newtype Response = Response
  { responseImages :: [ImageResponse]
  } deriving (Show, Generic)

instance A.ToJSON Response where
  toEncoding = A.genericToEncoding J.pixelJsonEncoding

--------------------------------------------------------------------------------

createImageResponse :: API.DigestText -> API.Image -> ImageResponse
createImageResponse uuid API.Image{..} = ImageResponse
  { imageResponsePath  = fold ["/static/images/", _imageHash]
  , imageResponseThumb = fold ["/static/thumbs/", _imageHash]
  , imageResponseUUID  = uuid
  }

--------------------------------------------------------------------------------

-- Fetch images from the backend, this endpoint accepts a filter to decide what
-- to return from the backend. By default this is all images ordered by upload
-- date and the filter should encompass all possible queries a user might have,
-- from tags to uploader, to ordering.
getImage :: Maybe API.Token -> C.Pixel Response
getImage Nothing  = throwError (E.ImageError API.MissingToken)
getImage (Just _) = handleImagesRquest

--------------------------------------------------------------------------------

handleImagesRquest
  :: Monad m
  => API.MonadImage m
  => m Response

handleImagesRquest = do
  images <- API.loadImages 10
  pure Response
    { responseImages = map (uncurry createImageResponse . first show) images
    }
