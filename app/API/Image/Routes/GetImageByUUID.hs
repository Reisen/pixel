module API.Image.Routes.GetImageByUUID
  ( GetImageByUUID
  , getImageByUUID
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
import qualified Data.UUID                     as U
import qualified Error                         as E
import qualified JSON                          as J

--------------------------------------------------------------------------------

-- We wrap up responses in an HTTP endpoint type, in order to abstract away
-- from the backend.
type GetImageByUUID =
  Header "Authorization" API.Token
    :> Capture "uuid" Text
    :> Get '[JSON] Response

--------------------------------------------------------------------------------

data Response = Response
  { responsePath  :: !Text
  , responseTags  :: !API.TagList
  , responseThumb :: !Text
  , responseUUID  :: !API.DigestText
  } deriving (Show, Generic)

instance A.ToJSON Response where
  toEncoding = J.pixelToEncoding
  toJSON     = J.pixelToJSON

--------------------------------------------------------------------------------

createImageResponse :: API.DigestText -> API.Image -> Response
createImageResponse uuid API.Image{..} = Response
  { responsePath  = fold ["/static/images/", _imageHash]
  , responseTags  = _imageTags
  , responseThumb = fold ["/static/thumbs/", _imageHash]
  , responseUUID  = uuid
  }

--------------------------------------------------------------------------------

getImageByUUID :: Maybe API.Token -> API.DigestText -> C.Pixel Response
getImageByUUID Nothing _     = throwError (E.ImageError API.MissingToken)
getImageByUUID (Just _) uuid = handleImageRequest uuid >>= \case
  Nothing       -> throwError (E.ImageError API.InvalidUUID)
  Just response -> pure response

--------------------------------------------------------------------------------

handleImageRequest
  :: Monad m
  => API.MonadImage m
  => API.DigestText
  -> m (Maybe Response)

handleImageRequest uuidText = case U.fromText uuidText of
  Nothing   -> pure Nothing
  Just uuid -> API.loadImage uuid >>= \case
    Nothing    -> pure Nothing
    Just image -> pure . Just $ createImageResponse uuidText image
