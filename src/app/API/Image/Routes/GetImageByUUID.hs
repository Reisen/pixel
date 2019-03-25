module API.Image.Routes.GetImageByUUID
  ( GetImageByUUID
  , getImageByUUID
  )
where

--------------------------------------------------------------------------------

import           Protolude
import           Servant

import           API.Image.Types                ( Image(..) )
import qualified Pixel                         as Pixel
import qualified MonadPixel                    as C

--------------------------------------------------------------------------------

-- We wrap up responses in an HTTP endpoint type, in order to abstract away
-- from the backend.
type GetImageByUUID =
  Header "Authorization" Pixel.Token
    :> Capture "uuid" Text
    :> Get '[JSON] Image

--------------------------------------------------------------------------------

convertImage
  :: Pixel.DigestText
  -> Pixel.Image
  -> Image

convertImage uuid Pixel.Image{..} = Image
  { imagePath  = fold ["/static/images/", _imageHash]
  , imageTags  = _imageTags
  , imageThumb = fold ["/static/thumbs/", _imageHash]
  , imageUUID  = uuid
  }

--------------------------------------------------------------------------------

getImageByUUID
  :: Maybe Pixel.Token
  -> Pixel.DigestText
  -> C.Pixel Image

getImageByUUID Nothing _     = throwError (Pixel.ImageError Pixel.MissingToken)
getImageByUUID (Just _) uuid =
  Pixel.handleImageRequest uuid >>= \case
    Nothing       -> throwError (Pixel.ImageError Pixel.InvalidUUID)
    Just response -> pure $ convertImage uuid response
