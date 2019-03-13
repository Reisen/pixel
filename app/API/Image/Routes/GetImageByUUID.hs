module API.Image.Routes.GetImageByUUID
  ( GetImageByUUID
  , getImageByUUID
  )
where

--------------------------------------------------------------------------------

import           Protolude
import           Servant

import qualified API.Image.Error               as API
import qualified Data.Aeson                    as A
import qualified Data.UUID                     as U
import qualified Error                         as E
import qualified JSON                          as J
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
  toEncoding = J.pixelToEncoding
  toJSON     = J.pixelToJSON

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

getImageByUUID Nothing _     = throwError (E.ImageError API.MissingToken)
getImageByUUID (Just _) uuid = handleImageRequest uuid >>= \case
  Nothing       -> throwError (E.ImageError API.InvalidUUID)
  Just response -> pure response

--------------------------------------------------------------------------------

handleImageRequest
  :: Monad m
  => Pixel.MonadImage m
  => Pixel.DigestText
  -> m (Maybe Response)

handleImageRequest uuidText = case U.fromText uuidText of
  Nothing   -> pure Nothing
  Just uuid -> Pixel.loadImage uuid >>= \case
    Nothing    -> pure Nothing
    Just image -> pure . Just $ createImageResponse uuidText image
