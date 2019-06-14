module API.Image.Routes.GetImage
  ( getImage
  ) where

import Protolude
import MonadPixel            ( Pixel )
import Pixel                 ( Error(..) )
import Pixel.API             ( CookieToken(..) )
import Pixel.API.FetchImages ( Response(..), APIImage(..) )
import Pixel.Model.Images    ( Image(..) )
import Pixel.Operations      ( findImages )

--------------------------------------------------------------------------------

-- Fetch images from the backend, this endpoint accepts a filter to decide what
-- to return from the backend. By default this is all images ordered by upload
-- date and the filter should encompass all possible queries a user might have,
-- from tags to uploader, to ordering.

getImage
  :: Maybe CookieToken
  -> Pixel Response

getImage = \case
  Nothing -> throwError UnknownError
  Just _  -> do
    images <- findImages
    pure . Response $ uncurry convertImage . first show <$> images


--------------------------------------------------------------------------------

convertImage
  :: Text
  -> Image
  -> APIImage

convertImage imageUUID Image{..} = APIImage
  { _dimensions = (0, 0)
  , _filename   = ""
  , _filesize   = 0
  , _path       = fold ["/static/images/", _hash]
  , _tags       = _tags
  , _thumb      = fold ["/static/thumbs/", _hash]
  , _uuid       = imageUUID
  }

