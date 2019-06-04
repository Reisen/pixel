module API.Image.Routes.GetTags
  ( getTags
  )
where

import Protolude
import Pixel              ( Error(..) )
import Pixel.API          ( FetchImageTagsResponse(..), CookieToken(..) )
import Pixel.Model.Images ( DigestText, ImageError(..), fetchTags )
import MonadPixel         ( Pixel )

--------------------------------------------------------------------------------

getTags
  :: Maybe CookieToken
  -> DigestText
  -> Pixel FetchImageTagsResponse

getTags Nothing _     = throwError (ImageError MissingToken)
getTags (Just _) uuid =
  fetchTags uuid >>= \case
    Nothing       -> throwError (ImageError InvalidUUID)
    Just response -> pure . FetchImageTagsResponse $ response
