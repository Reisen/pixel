module API.Image.Routes.GetImageByUUID
  ( getImageByUUID
  ) where

import Protolude
import Pixel              ( Error(..) )
import Pixel.API          ( FetchImageByUUIDResponse(..), CookieToken(..) )
import Pixel.Model.Images ( Image(..), DigestText, ImageError(..), handleImageRequest )
import MonadPixel         ( Pixel )

--------------------------------------------------------------------------------

getImageByUUID
  :: Maybe CookieToken
  -> DigestText
  -> Pixel FetchImageByUUIDResponse

getImageByUUID Nothing _     = throwError (ImageError MissingToken)
getImageByUUID (Just _) uuid =
  handleImageRequest uuid >>= \case
    Nothing       -> throwError (ImageError InvalidUUID)
    Just response -> pure $ convertImage uuid response

--------------------------------------------------------------------------------

convertImage
  :: DigestText
  -> Image
  -> FetchImageByUUIDResponse

convertImage uuid Image{..} = FetchImageByUUIDResponse
  { _dimensions = (0, 0)
  , _filename   = ""
  , _filesize   = 0
  , _path       = fold ["/static/images/", _hash]
  , _tags       = _tags
  , _thumb      = fold ["/static/thumbs/", _hash]
  , _uuid       = uuid
  }
