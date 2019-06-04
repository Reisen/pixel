module API.Image.Routes.GetTags
  ( getTags
  )
where

import Protolude
import Pixel                    ( Error(..) )
import Pixel.API                ( CookieToken(..) )
import Pixel.API.FetchImageTags ( Response(..) )
import Pixel.Model.Images       ( DigestText, ImageError(..), fetchTags )
import MonadPixel               ( Pixel )

--------------------------------------------------------------------------------

getTags
  :: Maybe CookieToken
  -> DigestText
  -> Pixel Response

getTags Nothing _     = throwError (ImageError MissingToken)
getTags (Just _) uuid =
  fetchTags uuid >>= \case
    Nothing       -> throwError (ImageError InvalidUUID)
    Just response -> pure . Response $ response
