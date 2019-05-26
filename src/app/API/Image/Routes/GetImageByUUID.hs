module API.Image.Routes.GetImageByUUID
  ( GetImageByUUID
  , getImageByUUID
  )
where

--------------------------------------------------------------------------------

import Protolude
import Servant

import API.Image.Types as API    ( Image(..) )
import Pixel                     ( Error(..) )
import Pixel.API.Token           ( Token )
import Pixel.API.Images as Pixel ( Image(..), DigestText, ImageError(..), handleImageRequest )
import MonadPixel                ( Pixel )

--------------------------------------------------------------------------------

-- We wrap up responses in an HTTP endpoint type, in order to abstract away
-- from the backend.
type GetImageByUUID =
  Header "Authorization" Token
    :> Capture "uuid" Text
    :> Get '[JSON] API.Image

--------------------------------------------------------------------------------

convertImage
  :: DigestText
  -> Pixel.Image
  -> API.Image

convertImage uuid Pixel.Image{..} = API.Image
  { imagePath  = fold ["/static/images/", _imageHash]
  , imageTags  = _imageTags
  , imageThumb = fold ["/static/thumbs/", _imageHash]
  , imageUUID  = uuid
  }

--------------------------------------------------------------------------------

getImageByUUID
  :: Maybe Token
  -> DigestText
  -> Pixel API.Image

getImageByUUID Nothing _     = throwError (ImageError MissingToken)
getImageByUUID (Just _) uuid =
  handleImageRequest uuid >>= \case
    Nothing       -> throwError (ImageError InvalidUUID)
    Just response -> pure $ convertImage uuid response
