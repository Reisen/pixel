module API.Image.Routes.GetImageByUUID
  ( getImageByUUID
  ) where

import Protolude
import Pixel                      ( Error(..) )
import Pixel.API                  ( CookieToken(..) )
import Pixel.API.FetchImageByUUID ( Response(..) )
import Pixel.Model.Images         ( Image(..), DigestText, ImageError(..), handleImageRequest )
import MonadPixel                 ( Pixel )

--------------------------------------------------------------------------------

getImageByUUID
  :: Maybe CookieToken
  -> DigestText
  -> Pixel Response

getImageByUUID Nothing _            = throwError (ImageError MissingToken)
getImageByUUID (Just _) imageUUID =
  handleImageRequest imageUUID >>= \case
    Nothing       -> throwError (ImageError InvalidUUID)
    Just response -> pure $ convertImage imageUUID response

--------------------------------------------------------------------------------

convertImage
  :: DigestText
  -> Image
  -> Response

convertImage imageUUID Image{..} = Response
  { _dimensions = (0, 0)
  , _filename   = ""
  , _filesize   = 0
  , _path       = fold ["/static/images/", _hash]
  , _tags       = _tags
  , _thumb      = fold ["/static/thumbs/", _hash]
  , _uuid       = imageUUID
  }
