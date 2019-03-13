module API.Image.Routes.GetImageByUUID
  ( GetImageByUUID
  , getImageByUUID
  )
where

--------------------------------------------------------------------------------

import           Protolude
import           Servant

import qualified Data.Aeson                    as A
import qualified Pixel                         as Pixel
import qualified MonadPixel                    as C

--------------------------------------------------------------------------------

-- We wrap up responses in an HTTP endpoint type, in order to abstract away
-- from the backend.
type GetImageByUUID =
  Header "Authorization" Pixel.Token
    :> Capture "uuid" Text
    :> Get '[JSON] Response

--------------------------------------------------------------------------------

data Response = Response
  { responsePath  :: !Text
  , responseTags  :: !Pixel.TagList
  , responseThumb :: !Text
  , responseUUID  :: !Pixel.DigestText
  } deriving (Show, Generic)

instance A.ToJSON Response where
  toEncoding = Pixel.pixelToEncoding
  toJSON     = Pixel.pixelToJSON

--------------------------------------------------------------------------------

createImageResponse
  :: Pixel.DigestText
  -> Pixel.Image
  -> Response

createImageResponse uuid Pixel.Image{..} = Response
  { responsePath  = fold ["/static/images/", _imageHash]
  , responseTags  = _imageTags
  , responseThumb = fold ["/static/thumbs/", _imageHash]
  , responseUUID  = uuid
  }

--------------------------------------------------------------------------------

getImageByUUID
  :: Maybe Pixel.Token
  -> Pixel.DigestText
  -> C.Pixel Response

getImageByUUID Nothing _     = throwError (Pixel.ImageError Pixel.MissingToken)
getImageByUUID (Just _) uuid =
  Pixel.handleImageRequest uuid >>= \case
    Nothing       -> throwError (Pixel.ImageError Pixel.InvalidUUID)
    Just response -> pure $ createImageResponse uuid response
