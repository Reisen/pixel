module API.Image.Routes.GetImageByUUID
  ( getImageByUUID
  ) where

import Protolude
import Pixel                      ( Error(..) )
import Pixel.API                  ( APIImage(..), CookieToken(..) )
import Pixel.API.FetchImageByUUID ( Response(..) )
import Pixel.Model.Images         ( Image(..) )
import Pixel.Operations           ( findImageByUUID )
import MonadPixel                 ( Pixel )

--------------------------------------------------------------------------------

getImageByUUID
  :: Maybe CookieToken
  -> Text
  -> Pixel Response

getImageByUUID Nothing _            = throwError UnknownError
getImageByUUID (Just _) imageUUID =
  findImageByUUID imageUUID >>= \case
    Nothing       -> throwError UnknownError
    Just response -> pure $ convertImage imageUUID response

--------------------------------------------------------------------------------

convertImage
  :: Text
  -> Image
  -> Response

convertImage imageUUID Image{..} = Response
  { _image = APIImage
    { _dimensions = (0, 0)
    , _filename   = ""
    , _filesize   = 0
    , _path       = fold ["/static/images/", _hash]
    , _tags       = _tags
    , _thumb      = fold ["/static/thumbs/", _hash]
    , _uuid       = imageUUID
    }
  }
