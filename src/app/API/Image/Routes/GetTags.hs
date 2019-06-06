module API.Image.Routes.GetTags
  ( getTags
  )
where

import Protolude
import Pixel                    ( Error(..) )
import Pixel.API                ( CookieToken(..) )
import Pixel.API.FetchImageTags ( Response(..) )
import Pixel.Model.Images       ( DigestText )
import Pixel.Operations         ( findTagsByUUID )
import MonadPixel               ( Pixel )

--------------------------------------------------------------------------------

getTags
  :: Maybe CookieToken
  -> DigestText
  -> Pixel Response

getTags Nothing _          = throwError UnknownError
getTags (Just _) imageUUID =
  findTagsByUUID imageUUID >>= \case
    Nothing       -> throwError UnknownError
    Just response -> pure . Response $ response
